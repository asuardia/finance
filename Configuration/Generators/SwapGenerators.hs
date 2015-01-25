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
import Data.List (zip4)
import qualified Data.Map as Map 
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars
import Configuration.Forex.Currencies
import Configuration.MktConventions.ScheduleGen as SG
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
type AssociationList = [(SwapMktData, String)]
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
                                    lgMarketData :: AssociationList
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
                                       lgMarketData :: AssociationList
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
                        lMarketData :: AssociationList,
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
                               lMarketData :: AssociationList,
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
    legs <- checkAllOk_ $ fmap (generateLeg stDate mat nom swGen (Just mainGen)) 
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
    legs <- checkAllOk_ $ fmap (generateLeg stDate mat nom swGen Nothing) 
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
    legs <- checkAllOk_ $ fmap (generateLeg stDate mat nom swGen (Just mainGen)) 
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
generateLeg :: StartDate -> CTP.Maturity -> CTP.Nominal -> SwapGenerator 
            -> Maybe LegGenerator -> LegGenerator -> Result_ Leg
          ---------------------------------
generateLeg stDate mat nom swapGen mbLg lg@FixedLegGen{} = do 
    let mainLg = if (isJust mbLg)
                 then fromJust mbLg
                 else lg
    let confSch = swgSchedules swapGen
    let sched = generateSchedule confSch mainLg lg
    --flows <- generateFlows swapGen stDate mat nom amort lg               
    return FixedLeg {
                        lPayReceive = lgPayReceive lg, 
                        lCurrency = lgCurrency lg, 
                        lStartDelay = lgStartDelay mainLg,
                        lPayCalendar = lgPayCalendar mainLg,
                        lSchedDef = sched,
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
    where generateSchedule :: ConfSchedules -> LegGenerator -> LegGenerator
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
generateLeg stDate mat nom swapGen mbLg lg@FloatingLegGen{} = do 
    let mainLg = if (isJust mbLg)
                 then fromJust mbLg
                 else lg
    let confSch = swgSchedules swapGen
    let sched = generateSchedule confSch mainLg lg
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
                           lSchedDef = sched,
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
    where generateSchedule :: ConfSchedules -> LegGenerator -> LegGenerator
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
generateFlows :: StartDate -> CTP.Maturity -> CTP.Nominal -> SwapGenerator 
              -> LegGenerator -> SchedDef -> Result_ [Flow] 
          ---------------------------------
generateFlows stDate mat nom swGen lg 
              fsch@FixedSchedDef {sdCalcStartSchedule = dSch@CTP.DrivingSchedule {}} 
    | isJust (CTP.matDate mat) = do     
        let matDt = fromJust $ CTP.matDate mat
        let dir = case (swgStubPeriod swGen) of UpFrontSP -> SG.Backward 
                                                InArrearsSP -> SG.Forward 
        let paySched = sdPaymentSchedule fsch
        let payFreq = sdPayFreqRatio fsch
        payCal <- idCalendar $ lgPayCalendar lg
        let stubDet = cspdCoupon $ lgStubPerDetail lg
        calcDts <- genDates stDate matDt dir dSch Nothing payFreq payCal stubDet  
        payDts <- genDates stDate matDt dir dSch (Just paySched) payFreq payCal stubDet
        payDts2 <- case (lgPayment lg) of CTP.InArrearsP -> Ok_ (tail payDts)
                                          CTP.UpFrontP -> Ok_ (init payDts)
                                          _ -> Error_ " generateFlows: option not implemented. " 
        remCaps <- genRemCap (swgDefaultAmort swGen) (CTP.nomQuantity nom) 
                             (length payDts2)   
        return (fmap (genFlow 0.0 (lgCurrency lg)) 
                     $ zip4 (init calcDts) (tail calcDts) payDts2 remCaps)
        
    | otherwise = do
        let aproxDt = getMatDate stDate mat
        let matDt = addDays 600 aproxDt 
        let paySched = sdPaymentSchedule fsch
        let payFreq = sdPayFreqRatio fsch
        payCal <- idCalendar $ lgPayCalendar lg
        let stubDet = cspdCoupon $ lgStubPerDetail lg
        calcDts <- genDates stDate matDt SG.Forward dSch Nothing payFreq payCal stubDet  
        payDts <- genDates stDate matDt SG.Forward dSch (Just paySched) payFreq payCal stubDet
        payDts2 <- case (lgPayment lg) of CTP.InArrearsP -> Ok_ (tail payDts)
                                          CTP.UpFrontP -> Ok_ (init payDts)
                                          _ -> Error_ " generateFlows: option not implemented. " 
        remCaps <- genRemCap (swgDefaultAmort swGen) (CTP.nomQuantity nom) 
                             (length payDts2)    
        let limit = findLimit aproxDt calcDts
        return (fmap (genFlow 0.0 (lgCurrency lg)) 
                     $ zip4 (take limit $ init calcDts) 
                            (take limit $ tail calcDts) 
                            (take limit $ payDts2) 
                            (take limit $ remCaps))
    where genFlow :: Double -> CurrencyLabel -> (Day, Day, Day, Double) 
                  -> Flow
          genFlow rate curr (st, nd, pd, cap) 
              = FixedFlow {
                             ffCalcStartDate = st,
                             ffCalcEndDate = nd,
                             ffRemCapital = cap,
                             ffRate = rate,
                             ffPayDate = pd,
                             ffFlow = cap * rate ,
                             ffCurr = curr
                          }
          findLimit aproxDt calcDts = let diffs = fmap (diffDays aproxDt) calcDts 
                                          limitElem = minimum diffs 
                                      in (length $ takeWhile ((>) limitElem) diffs) - 1
          ---------------------------------
generateFlows _ _ _ _ _ _ = Error_ " generateFlows: option not implemented. "  
--------------------------------------------------------------------------
genRemCap :: Amortizing -> Double -> Int -> Result_ [Double]
genRemCap NoneA iniCap nbFlows = Ok_ (replicate nbFlows iniCap)
genRemCap _ _ _ = Error_ " genRemCap: option not implemented. " 

--------------------------------------------------------------------------

getMatDate :: StartDate -> CTP.Maturity -> Day             
getMatDate stDate mat = if isJust (CTP.matDate mat)
                        then fromJust $ CTP.matDate mat
                        else let unit = CTP.tenorUnit (fromJust $ CTP.matTenor mat)
                                 nbUnits = CTP.tenorQuantity (fromJust $ CTP.matTenor mat)
                             in shiftDate' unit False stDate nbUnits     
--------------------------------------------------------------------------
genDates :: Day -> Day -> GenerationFreq -> CTP.Schedule -> Maybe CTP.Schedule
             -> Int -> Calendar -> StubCoupon -> Result_ [Day]
          ---------------------------------
genDates stDate matDt fwd@SG.Forward 
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         (Just CTP.SchEqual2{ CTP.equal2 = CTP.SchStart})
         freq cal stubDet
    = genDates stDate matDt fwd stSch Nothing freq cal stubDet
          ---------------------------------
genDates stDate matDt SG.Forward 
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         (Just CTP.SchDeducedFrom{
                                    CTP.schDedFrom = CTP.SchStart,
                                    CTP.schDedForm = dtShiftLabel
                                })  
         freq cal stubDet
    = do
    schedGenAux <- (idScheduleGen schedGenLab) 
    let schedGen = schedGenAux {
                                   sgCalendCheck = None,
                                   sgGenFrequency = SG.Forward
                               }                                       
    let matDt2 = addDays 1 matDt
    calcDts <- genSchedule schedGen Nothing stDate matDt2
    let calcDts2 = (stDate:calcDts)
    let calcDts3 =  calcDts2--now check the frequency and remove dates       
    let isNotStub = matDt `elem` calcDts3
    calcDts4 <- if isNotStub then Ok_ calcDts3
                else case stubDet of ShortCoupon -> Ok_ (calcDts3 ++ [matDt]) 
                                     LongCoupon -> Ok_ ((init calcDts3) ++ [matDt]) 
                                     _ -> Error_ " genDates: option not implemented. " 
    dtShifter <- idDateShifter dtShiftLabel                   
    checkAllOk_ $ fmap (shiftDate dtShifter (Just cal)) calcDts4
          ---------------------------------
genDates stDate matDt SG.Forward
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         Nothing freq cal stubDet
    = do
    schedGenAux <- (idScheduleGen schedGenLab) 
    let schedGen = schedGenAux {
                                   sgGenFrequency = SG.Forward
                               }
    let matDt2 = addDays 1 matDt
    calcDts <- genSchedule schedGen (Just cal) stDate matDt2
    let calcDts2 = (checkingCal External (Just cal) (sgRollConv schedGen) stDate):calcDts
    let calcDts3 =  calcDts2--now check the frequency and remove dates       
    let isNotStub = matDt `elem` calcDts3
    calcDts4 <- if isNotStub then Ok_ calcDts3
                else case stubDet of ShortCoupon -> Ok_ (calcDts3 ++ [(checkingCal External (Just cal) (sgRollConv schedGen) matDt)]) 
                                     LongCoupon -> Ok_ ((init calcDts3) ++ [(checkingCal External (Just cal) (sgRollConv schedGen) matDt)]) 
                                     _ -> Error_ " genDates: option not implemented. " 
    return calcDts4
          ---------------------------------
genDates stDate matDt bwd@SG.Backward 
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         (Just CTP.SchEqual2{ CTP.equal2 = CTP.SchStart})
         freq cal stubDet
    = genDates stDate matDt bwd stSch Nothing freq cal stubDet
          ---------------------------------
genDates stDate matDt SG.Backward 
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         (Just CTP.SchDeducedFrom{
                                    CTP.schDedFrom = CTP.SchStart,
                                    CTP.schDedForm = dtShiftLabel
                                })  
         freq cal stubDet
    = do
    schedGenAux <- (idScheduleGen schedGenLab) 
    let schedGen = schedGenAux {
                                   sgCalendCheck = None,
                                   sgGenFrequency = SG.Backward
                               }   
    let stDate2 = addDays (-1) stDate
    calcDts <- genSchedule schedGen Nothing stDate2 matDt
    let calcDts2 = calcDts ++ [matDt]
    let calcDts3 =  calcDts2--now check the frequency and remove dates       
    let isNotStub = stDate `elem` calcDts3
    calcDts4 <- if isNotStub then Ok_ calcDts3
                else case stubDet of ShortCoupon -> Ok_ (stDate:calcDts3) 
                                     LongCoupon -> Ok_ (stDate:(tail calcDts3)) 
                                     _ -> Error_ " genDates: option not implemented. " 
    dtShifter <- idDateShifter dtShiftLabel                   
    checkAllOk_ $ fmap (shiftDate dtShifter (Just cal)) calcDts4
          --------------------------------- 
genDates stDate matDt SG.Backward
         stSch@CTP.DrivingSchedule {
                                       CTP.schedule = schedGenLab
                                   } 
         Nothing freq cal stubDet
    = do
    schedGenAux <- (idScheduleGen schedGenLab) 
    let schedGen = schedGenAux {
                                   sgGenFrequency = SG.Backward
                               }
    let stDate2 = addDays (-1) stDate
    calcDts <- genSchedule schedGen (Just cal) stDate2 matDt
    let calcDts2 =  calcDts ++ [checkingCal External (Just cal) (sgRollConv schedGen) matDt]
    let calcDts3 =  calcDts2--now check the frequency and remove dates       
    let isNotStub = stDate `elem` calcDts3
    calcDts4 <- if isNotStub then Ok_ calcDts3
                else case stubDet of ShortCoupon -> Ok_ ((checkingCal External (Just cal) (sgRollConv schedGen) stDate):calcDts3) 
                                     LongCoupon -> Ok_ ((checkingCal External (Just cal) (sgRollConv schedGen) stDate):(tail calcDts3)) 
                                     _ -> Error_ " genDates: option not implemented. " 
    return calcDts4 
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
                                      lgMarketData = [(mkdDiscCurve, "EUR_NO_COL_DISC")]
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
                                            lgMarketData = [(mkdDiscCurve, "EUR_NO_COL_DISC"), (mkdEstCurve, "EUR_FUTSWAP_3M")]
                                  }
-------------------------------------------------------------------------- 

example1 = generateSwap sgEUR_IBOR_3M (fromGregorian 2015 01 20) CTP.Maturity {CTP.matTenor = Nothing, CTP.matDate = Just (fromGregorian 2020 01 20)} CTP.Nominal {CTP.nomQuantity = 100000000, CTP.nomCurrency = eur}                             

prueba = do
    let v = (\(Ok_ k) -> k) example1    
    writeFile "./output.json" (encodeJSON v)
