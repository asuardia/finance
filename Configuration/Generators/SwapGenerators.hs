{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -XGADTs #-}
--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Generators.SwapGenerators
    (    
     SwapGenerator (..), LegGenerator (..), Swap (..),
     Leg (..), Flow (..), SwapMktData (..),
     generateSwap
     --sgEUR_IBOR_3M
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Data.Maybe
import qualified Data.Map as Map 
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars
import Configuration.Forex.Currencies
import Configuration.MktConventions.ScheduleGen
import Configuration.MktConventions.DateShifters
import Configuration.MktConventions.RateConv
import qualified Configuration.CommonTypes.Types as CT
import qualified Configuration.CommonTypes.TypesProducts as CTP
import Configuration.Indices.IRIndices
import Valuation.Analytical  
import Valuation.MktData  
import Valuation
--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type StartDate = Day
type SwapMktData = String
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data SwapGenerator = SwapGenerator { 
                                       swgDescription :: String,
                                       swgEvaluation :: SwapEvaluation,
                                       swgEstimation :: Bool,
                                       swgNumberLegs :: Int,
                                       swgSchedules :: ConfSchedules,
                                       swgStubPeriod :: ConfStubPeriod,
                                       swgSettlementDelay :: ConfSettlementDelay,
                                       swgDefaultAmort :: Amortizing,
                                       --swgFutCashProccedCutOff :: FurCashProcCutOff,
                                       swgMarketQuote :: ConfMarketQuote,
                                       swgLegs :: [LegGenerator]
                                   } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data SwapEvaluation = DefaultSE | MtMSE | AccrualSE 
                      deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ConfSchedules = CommSets | IndSets | CommSetsDiffFreq | IndSetsCommCap 
                     deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ConfStubPeriod = UpFrontSP | InArrearsSP | BothEndsSP 
                      deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ConfSettlementDelay = InheritedFromCurr 
                         | OthersCSD deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Amortizing = NoneA 
                | LinearRate {
                                 aFrequency :: CTP.Frequency,
                                 aRate :: Double
                             }                             
                | Constant {
                               aFrequency :: CTP.Frequency,
                               aEndNominal :: Double
                           }                    
                | LinearAmount {
                                   aFrequency :: CTP.Frequency,
                                   aAmount :: Double
                               }deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
--data FurCashProcCutOff = Automatic 
--                       | OthersCMQ deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ConfMarketQuote = AutomaticCMQ 
                     | OthersCMQ deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data LegGenerator = FixedLegGen {
                                    lgIsMainLeg :: Bool,
                                    lgPayReceive :: CTP.PayReceive, 
                                    lgCurrency :: CurrencyLabel, 
                                    lgStartDelay :: DateShifterLabel,
                                    lgPayCalendar :: CalendarLabel,
                                    lgSchedDef :: SchedDef,
                                    lgPayment :: CTP.Payment,
                                    lgRateConv :: RateConvLabel, 
                                    lgRounding :: CT.RoundingRule,
                                    lgStubPerDetail :: ConfStubPerDetail,
                                    lgDayCount :: DayCount,
                                    lgIniExchange :: Bool,
                                    lgIntermPayments :: Bool,
                                    lgFinExchange :: Bool,
                                    lgAccrualConv :: AccrualConv,
                                    lgYieldConv :: YieldConv,
                                    lgMarketData :: Map.Map SwapMktData String
                                } 
                  | FloatingLegGen {
                                       lgIsMainLeg :: Bool,
                                       lgPayReceive :: CTP.PayReceive, 
                                       lgIRIndex :: IRIndexLabel, 
                                       lgCurrency :: CurrencyLabel, 
                                       lgStartDelay :: DateShifterLabel,
                                       lgPayCalendar :: CalendarLabel,
                                       lgFixCalendar :: CalendarLabel,
                                       lgSchedDef :: SchedDef,
                                       lgFixing :: CTP.Fixing,
                                       lgPayment :: CTP.Payment,
                                       lgRateConv :: RateConvLabel, 
                                       lgRounding :: CT.RoundingRule,
                                       lgStubPerDetail :: ConfStubPerDetail,
                                       lgMarginMode :: MarginMode,
                                       lgDayCount :: DayCount,
                                       lgIniExchange :: Bool,
                                       lgIntermPayments :: Bool,
                                       lgFinExchange :: Bool,
                                       lgAccrualConv :: AccrualConv,
                                       lgYieldConv :: YieldConv,
                                       lgMarketData :: Map.Map SwapMktData String
                                   } 
                    deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data SchedDef = FixedSchedDef {
                                  sdCalcStartSchedule :: CTP.Schedule,
                                  sdCalcEndSchedule   :: CTP.Schedule,
                                  sdPaymentSchedule   :: CTP.Schedule,
                                  sdPayFreqRatio      :: Int,
                                  sdCapitalSchedule   :: CTP.Schedule,
                                  sdCapFreqRatio      :: Int
                              }
              | FloatSchedDef {
                                  sdCalcStartSchedule :: CTP.Schedule,
                                  sdCalcEndSchedule   :: CTP.Schedule,
                                  sdPaymentSchedule   :: CTP.Schedule,
                                  sdPayFreqRatio      :: Int,
                                  sdFixingSchedule    :: CTP.Schedule,
                                  sdFixFreqRatio      :: Int,
                                  sdCapitalSchedule   :: CTP.Schedule,
                                  sdCapFreqRatio      :: Int
                              }
                deriving (Eq, Show, Data, Typeable)        
--------------------------------------------------------------------------
data DayCount = FixedDayCount { dcActive :: Bool}
              | FloatDayCount {
                                  dcActive :: Bool,
                                  dcIncludeMargin :: Bool
                              }
                deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------
data ConfStubPerDetail = FixedConfStubPerDetail {
                                                    cspdCoupon :: StubCoupon
                                                }
                       | FloatConfStubPerDetail {
                                                    cspdCoupon :: StubCoupon,
                                                    cspdStubPerRate :: StubPerRate
                                                }      
                       deriving (Eq, Show, Data, Typeable)                                               
--------------------------------------------------------------------------
data StubCoupon = ShortCoupon | LongCoupon | FullCoupon | Conditional
                  deriving (Eq, Show, Data, Typeable)                                               
--------------------------------------------------------------------------
data StubPerRate = CurrentIndex | ClosestIndex | NextIndex | InterpIndex
                  deriving (Eq, Show, Data, Typeable)                                           
--------------------------------------------------------------------------
data MarginMode = AdditiveMM | MultiplicativeMM 
                | InUnderlyingMM | FactorMM
                  deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------
data AccrualConv = AccrualConv {
                                   acMethod :: AccrualMethod,
                                   acRounding :: AccrualRounding,
                                   acRoundMode :: AccrualRoundingMode
                               }      
                   deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------
data AccrualMethod = UseInterestConv | OthersAcrMethods
                     deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------
data AccrualRounding = NoneAR | OthersAR
                       deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------
data AccrualRoundingMode = StandardARM | OthersARM
                           deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------
data YieldConv = YieldConv {
                               ycCalculation :: YieldCalc,
                               ycCoumponding :: YieldCoumpond,
                               ycGrossToCleanAccrual :: YieldGross2CleanAccr,
                               ycAltYieldConvention :: YieldAltYieldConv
                           }      
                 deriving (Eq, Show, Data, Typeable)    
--------------------------------------------------------------------------
data YieldCalc = AIBD | OthersYC deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data YieldCoumpond = AtCouponFreq | OthersYCoump 
                     deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data YieldGross2CleanAccr = StandardGross2CleanAccr | OthersGCA
                            deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data YieldAltYieldConv = NoAYC | OthersAYC
                         deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Swap = Swap {
                     swGenerator :: SwapGenerator,
                     swStartDate :: Day,
                     swMaturity :: CTP.Maturity,
                     swNominal :: CTP.Nominal,                     
                     swEvaluation :: SwapEvaluation,
                     swEstimation :: Bool,
                     swNumberLegs :: Int,
                     swSchedules :: ConfSchedules,
                     swStubPeriod :: ConfStubPeriod,
                     swSettlementDelay :: ConfSettlementDelay,
                     swAmortizing :: Amortizing,
                     --swFutCashProccedCutOff :: FurCashProcCutOff,
                     swMarketQuote :: ConfMarketQuote,
                     swAddFlows :: Maybe [AdFlow],
                     swLegs :: [Leg]
                 } deriving (Eq, Show, Data, Typeable)            
--------------------------------------------------------------------------
data AdFlow = AdFlow {
                         flDate :: Day,
                         flQuantity :: CTP.Nominal
                     } deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------
data Leg = FixedLeg {
                        lPayReceive :: CTP.PayReceive, 
                        lCurrency :: CurrencyLabel, 
                        lStartDelay :: DateShifterLabel,
                        lPayCalendar :: CalendarLabel,
                        lSchedDef :: SchedDef,
                        lPayment :: CTP.Payment,
                        lRateConv :: RateConvLabel, 
                        lRounding :: CT.RoundingRule,
                        lStubPerDetail :: ConfStubPerDetail,
                        lDayCount :: DayCount,
                        lIniExchange :: Bool,
                        lIntermPayments :: Bool,
                        lFinExchange :: Bool,
                        lAccrualConv :: AccrualConv,
                        lYieldConv :: YieldConv,
                        lMarketData :: Map.Map SwapMktData String,
                        lRate :: Double,
                        lFlows :: [Flow]
                    } 
             | FloatingLeg {
                               lPayReceive :: CTP.PayReceive, 
                               lIRIndex :: IRIndexLabel, 
                               lFactor :: Int, 
                               lCurrency :: CurrencyLabel,
                               lStartDelay :: DateShifterLabel,
                               lFirstFix :: Double,
                               lRateConv :: RateConvLabel, 
                               lMargin :: Double,
                               lPayCalendar :: CalendarLabel,
                               lFixCalendar :: CalendarLabel,
                               lSchedDef :: SchedDef,
                               lFixing :: CTP.Fixing,
                               lPayment :: CTP.Payment,
                               lRounding :: CT.RoundingRule,
                               lStubPerDetail :: ConfStubPerDetail,
                               lMarginMode :: MarginMode,
                               lDayCount :: DayCount,
                               lIniExchange :: Bool,
                               lIntermPayments :: Bool,
                               lFinExchange :: Bool,
                               lAccrualConv :: AccrualConv,
                               lYieldConv :: YieldConv,
                               lMarketData :: Map.Map SwapMktData String,
                               lFlows :: [Flow]
                           } deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------

data Flow = FixedFlow {
                           ffCalcStartDate :: Day,
                           ffCalcEndDate :: Day,
                           ffRemCapital :: Double,
                           ffRate :: Double,
                           ffPayDate :: Day,
                           ffFlow :: Double,
                           ffCurr :: CurrencyLabel
                       } 
          | FloatingFlow {
                             flfCalcStartDate :: Day,
                             flfCalcEndDate :: Day,
                             flfRemCapital :: Double,
                             flfFixDate :: Day,
                             flfRate :: Double,
                             flfMargin :: Double,
                             flfRateFactor :: Double,
                             flfPayDate :: Day,
                             flfFlow :: Double,
                             flfCurr :: CurrencyLabel
                         } deriving (Eq, Show, Data, Typeable)             
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
generateSwap :: SwapGenerator -> StartDate -> CTP.Maturity -> CTP.Nominal
             -> Result_ Swap
          ---------------------------------
generateSwap swGen@SwapGenerator{swgSchedules = cs@CommSets} 
             stDate mat nom = do
    --let stDate2 = cnfSettDelay stDate
    let amort = swgDefaultAmort swGen
    mainGen <- searchMainGen (swgLegs swGen)
    legs <- checkAllOk_ $ fmap (generateLeg cs stDate mat nom (Just mainGen) amort) 
                               (swgLegs swGen)
    return Swap {
                    swGenerator = swGen,
                    swStartDate = stDate,
                    swMaturity = mat,
                    swNominal = nom,                     
                    swEvaluation = swgEvaluation swGen,
                    swEstimation = swgEstimation swGen,
                    swNumberLegs = swgNumberLegs swGen,
                    swSchedules = swgSchedules swGen,
                    swStubPeriod = swgStubPeriod swGen,
                    swSettlementDelay = swgSettlementDelay swGen,
                    swAmortizing = amort,
                    swMarketQuote = swgMarketQuote swGen,
                    swAddFlows = Nothing,
                    swLegs = legs
                }
          ---------------------------------
generateSwap swGen@SwapGenerator{swgSchedules = cs@IndSets} 
             stDate mat nom = do
    --let stDate2 = cnfSettDelay stDate
    let amort = swgDefaultAmort swGen
    legs <- checkAllOk_ $ fmap (generateLeg cs stDate mat nom Nothing amort) 
                               (swgLegs swGen)
    return Swap {
                    swGenerator = swGen,
                    swStartDate = stDate,
                    swMaturity = mat,
                    swNominal = nom,                     
                    swEvaluation = swgEvaluation swGen,
                    swEstimation = swgEstimation swGen,
                    swNumberLegs = swgNumberLegs swGen,
                    swSchedules = swgSchedules swGen,
                    swStubPeriod = swgStubPeriod swGen,
                    swSettlementDelay = swgSettlementDelay swGen,
                    swAmortizing = amort,
                    swMarketQuote = swgMarketQuote swGen,
                    swAddFlows = Nothing,
                    swLegs = legs
                }
          ---------------------------------
generateSwap swGen@SwapGenerator{swgSchedules = cs@CommSetsDiffFreq} 
             stDate mat nom = do
    --let stDate2 = cnfSettDelay stDate
    let amort = swgDefaultAmort swGen
    mainGen <- searchMainGen (swgLegs swGen)
    legs <- checkAllOk_ $ fmap (generateLeg cs stDate mat nom (Just mainGen) amort) 
                               (swgLegs swGen)
    return Swap {
                    swGenerator = swGen,
                    swStartDate = stDate,
                    swMaturity = mat,
                    swNominal = nom,                     
                    swEvaluation = swgEvaluation swGen,
                    swEstimation = swgEstimation swGen,
                    swNumberLegs = swgNumberLegs swGen,
                    swSchedules = swgSchedules swGen,
                    swStubPeriod = swgStubPeriod swGen,
                    swSettlementDelay = swgSettlementDelay swGen,
                    swAmortizing = amort,
                    swMarketQuote = swgMarketQuote swGen,
                    swAddFlows = Nothing,
                    swLegs = legs
                }
          ---------------------------------
generateSwap _ _ _ _ = Error_ " generateSwap: option not implemented. "
--------------------------------------------------------------------------
searchMainGen :: [LegGenerator] -> Result_ LegGenerator
searchMainGen (lg:lgs) = if (lgIsMainLeg lg)
                         then Ok_ lg
                         else searchMainGen lgs
searchMainGen [] = Error_ " generateSwap: leg generator not found. "
--------------------------------------------------------------------------
generateLeg :: ConfSchedules -> StartDate -> CTP.Maturity -> CTP.Nominal 
            -> Maybe LegGenerator -> Amortizing -> LegGenerator -> Result_ Leg
          ---------------------------------
generateLeg confSch stDate mat nom mbLg amort lg@FixedLegGen{} = do 
    let mainLg = if (isJust mbLg)
                 then fromJust mbLg
                 else lg
    --flows <- generateFlows stDate mat nom amort lg               
    return FixedLeg {
                        lPayReceive = lgPayReceive lg, 
                        lCurrency = lgCurrency lg, 
                        lStartDelay = lgStartDelay mainLg,
                        lPayCalendar = lgPayCalendar mainLg,
                        lSchedDef = generateSchedule confSch mainLg lg,
                        lPayment = lgPayment lg,
                        lRateConv = lgRateConv lg, 
                        lRounding = lgRounding lg,
                        lStubPerDetail = lgStubPerDetail lg,
                        lDayCount = lgDayCount lg,
                        lIniExchange = lgIniExchange lg,
                        lIntermPayments = lgIntermPayments lg,
                        lFinExchange = lgFinExchange lg,
                        lAccrualConv = lgAccrualConv lg,
                        lYieldConv = lgYieldConv lg,
                        lMarketData = lgMarketData lg,
                        lRate = 0.0,
                        lFlows = []
                    }
    where generateFlows :: StartDate -> CTP.Maturity -> CTP.Nominal 
                        -> Amortizing -> LegGenerator -> Result_ [Flow] 
          generateFlows stDate mat nom amort lg = Error_ ""
          generateSchedule :: ConfSchedules -> LegGenerator -> LegGenerator
                           -> SchedDef
          generateSchedule IndSets mainLg lg = lgSchedDef lg
          generateSchedule CommSets mainLg lg = lgSchedDef mainLg
          generateSchedule CommSetsDiffFreq mainLg lg = 
              FixedSchedDef {
                                sdCalcStartSchedule = sdCalcStartSchedule $ lgSchedDef mainLg,
                                sdCalcEndSchedule   = sdCalcEndSchedule $ lgSchedDef mainLg,
                                sdPaymentSchedule   = sdPaymentSchedule $ lgSchedDef mainLg,
                                sdPayFreqRatio      = sdPayFreqRatio $ lgSchedDef lg,
                                sdCapitalSchedule   = sdCapitalSchedule $ lgSchedDef mainLg,
                                sdCapFreqRatio      = sdCapFreqRatio $ lgSchedDef lg
                            }
          ---------------------------------
generateLeg confSch stDate mat nom mbLg amort lg@FloatingLegGen{} = do 
    let mainLg = if (isJust mbLg)
                 then fromJust mbLg
                 else lg
    --flows <- generateFlows stDate mat nom amort lg           
    return FloatingLeg {
                           lPayReceive = lgPayReceive lg, 
                           lIRIndex = lgIRIndex lg, 
                           lFactor = 1, 
                           lCurrency = lgCurrency lg, 
                           lStartDelay = lgStartDelay mainLg,
                           lFirstFix = 0.0,
                           lMargin = 0.0,
                           lPayCalendar = lgPayCalendar mainLg,
                           lFixCalendar = lgFixCalendar mainLg,
                           lSchedDef = generateSchedule confSch mainLg lg,
                           lFixing = lgFixing lg,
                           lPayment = lgPayment lg,
                           lRateConv = lgRateConv lg, 
                           lRounding = lgRounding lg,
                           lStubPerDetail = lgStubPerDetail lg,
                           lMarginMode = lgMarginMode lg,
                           lDayCount = lgDayCount lg,
                           lIniExchange = lgIniExchange lg,
                           lIntermPayments = lgIntermPayments lg,
                           lFinExchange = lgFinExchange lg,
                           lAccrualConv = lgAccrualConv lg,
                           lYieldConv = lgYieldConv lg,
                           lMarketData = lgMarketData lg,
                           lFlows = []
                       }
    where generateFlows :: StartDate -> CTP.Maturity -> CTP.Nominal 
                        -> Amortizing -> LegGenerator -> Result_ [Flow] 
          generateFlows stDate mat nom amort lg = Error_ ""
          generateSchedule :: ConfSchedules -> LegGenerator -> LegGenerator
                           -> SchedDef
          generateSchedule IndSets mainLg lg = lgSchedDef lg
          generateSchedule CommSets mainLg lg = lgSchedDef mainLg
          generateSchedule CommSetsDiffFreq mainLg lg = 
              FloatSchedDef {
                                sdCalcStartSchedule = sdCalcStartSchedule $ lgSchedDef mainLg,
                                sdCalcEndSchedule   = sdCalcEndSchedule $ lgSchedDef mainLg,
                                sdPaymentSchedule   = sdPaymentSchedule $ lgSchedDef mainLg,
                                sdPayFreqRatio      = sdPayFreqRatio $ lgSchedDef lg,
                                sdFixingSchedule    = sdFixingSchedule $ lgSchedDef mainLg,
                                sdFixFreqRatio      = sdFixFreqRatio $ lgSchedDef lg,
                                sdCapitalSchedule   = sdCapitalSchedule $ lgSchedDef mainLg,
                                sdCapFreqRatio      = sdCapFreqRatio $ lgSchedDef lg
                            }

--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------



--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
mkdEstCurve = "EstCurve" 
mkdDiscCurve = "DiscCurve"
mkdCapFloorVol = "CapFloorVol"
mkdSwaptionVol = "SwaptionVol"        
-------------------------------------------------------------------------- 
sgEUR_IBOR_3M = SwapGenerator { 
                                  swgDescription = "EUR-IBOR-3M",
                                  swgEvaluation = DefaultSE,
                                  swgEstimation = True,
                                  swgNumberLegs = 2,
                                  swgSchedules = CommSetsDiffFreq,
                                  swgStubPeriod = UpFrontSP,
                                  swgSettlementDelay = InheritedFromCurr,
                                  swgDefaultAmort = NoneA,
                                  --swgFutCashProccedCutOff :: FurCashProcCutOff,
                                  swgMarketQuote = AutomaticCMQ,
                                  swgLegs = [swgFixLeg, swgFloatLeg]
                              } 
    where swgFixLeg = FixedLegGen {
                                      lgIsMainLeg = False,
                                      lgPayReceive = CTP.Pay, 
                                      lgCurrency = cEUR, 
                                      lgStartDelay = cTARGET,
                                      lgPayCalendar = cTARGET,
                                      lgSchedDef = FixedSchedDef {
                                                                      sdCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = sg3M_MODFOLL},
                                                                      sdCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                      sdPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                      sdPayFreqRatio      = 4,
                                                                      sdCapitalSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                      sdCapFreqRatio      = 4
                                                                  },
                                      lgPayment = CTP.InArrearsP,
                                      lgRateConv = rcLIN_30360, 
                                      lgRounding = CT.None,
                                      lgStubPerDetail = FixedConfStubPerDetail {
                                                                                   cspdCoupon = ShortCoupon
                                                                               },
                                      lgDayCount = FixedDayCount {dcActive = True},
                                      lgIniExchange = False,
                                      lgIntermPayments = False,
                                      lgFinExchange = False,
                                      lgAccrualConv = AccrualConv {
                                                                      acMethod = UseInterestConv,
                                                                      acRounding = NoneAR,
                                                                      acRoundMode = StandardARM
                                                                  },
                                      lgYieldConv = YieldConv {
                                                                  ycCalculation = AIBD,
                                                                  ycCoumponding = AtCouponFreq,
                                                                  ycGrossToCleanAccrual = StandardGross2CleanAccr,
                                                                  ycAltYieldConvention = NoAYC
                                                              }   ,
                                      lgMarketData = Map.fromList [(mkdDiscCurve, "EUR_NO_COL_DISC")]
                                  }
          swgFloatLeg = FloatingLegGen  {
                                            lgIsMainLeg = True,
                                            lgPayReceive = CTP.Receive,  
                                            lgIRIndex = iEURIBOR3M, 
                                            lgCurrency = cEUR, 
                                            lgStartDelay = dsPLUS_2_OPEN_DAYS,
                                            lgPayCalendar = cTARGET,
                                            lgFixCalendar = cTARGET,
                                            lgSchedDef = FloatSchedDef {
                                                                            sdCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = sg3M_MODFOLL},
                                                                            sdCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                            sdPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                            sdPayFreqRatio      = 1,
                                                                            sdFixingSchedule    = CTP.SchDeducedFrom {
                                                                                                                          CTP.schDedFrom = CTP.SchStart,
                                                                                                                          CTP.schDedForm = dsMINUS_2_OPEN_DAYS
                                                                                                                      },
                                                                            sdFixFreqRatio      = 1,
                                                                            sdCapitalSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                            sdCapFreqRatio      = 1
                                                                        },     
                                            lgFixing = CTP.UpFrontFix,
                                            lgPayment = CTP.InArrearsP,
                                            lgRateConv = rcLIN_ACT360, 
                                            lgRounding = CT.None,
                                            lgStubPerDetail = FloatConfStubPerDetail {
                                                                                         cspdCoupon = ShortCoupon,
                                                                                         cspdStubPerRate = CurrentIndex
                                                                                     },
                                            lgMarginMode = AdditiveMM,
                                            lgDayCount = FloatDayCount {
                                                                           dcActive = True,
                                                                           dcIncludeMargin = True
                                                                       },
                                            lgIniExchange = False,
                                            lgIntermPayments = False,
                                            lgFinExchange = False,
                                            lgAccrualConv = AccrualConv {
                                                                            acMethod = UseInterestConv,
                                                                            acRounding = NoneAR,
                                                                            acRoundMode = StandardARM
                                                                        },
                                            lgYieldConv = YieldConv {
                                                                        ycCalculation = AIBD,
                                                                        ycCoumponding = AtCouponFreq,
                                                                        ycGrossToCleanAccrual = StandardGross2CleanAccr,
                                                                        ycAltYieldConvention = NoAYC
                                                                  }   ,
                                            lgMarketData = Map.fromList [(mkdDiscCurve, "EUR_NO_COL_DISC"), (mkdEstCurve, "EUR_FUTSWAP_3M")]
                                  }
-------------------------------------------------------------------------- 

example1 = generateSwap sgEUR_IBOR_3M (fromGregorian 2015 01 20) CTP.Maturity {CTP.matTenor = Nothing, CTP.matDate = Just (fromGregorian 2020 01 20)} CTP.Nominal {CTP.nomQuantity = 100000000, CTP.nomCurrency = eur}                             

