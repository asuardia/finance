{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Generators.SwapGenerators
    (    
     SwapGenerator (..), LegGenerator (..), Swap (..),
     Leg (..), Flow (..), SwapMktData (..)
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
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
import Valuation
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
                                       swgDefaultAmort :: DefaultAmortizing,
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
data DefaultAmortizing = NoneDA 
                       | OthersDA deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
--data FurCashProcCutOff = Automatic 
--                       | OthersCMQ deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ConfMarketQuote = AutomaticCMQ 
                     | OthersCMQ deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data LegGenerator = FixedLegGen {
                                    lgPayReceive :: CTP.PayReceive, 
                                    lgCurrency :: Currency, 
                                    lgStartDelay :: Maybe DateShifter,
                                    lgPayCalendar :: Maybe Calendar,
                                    lgSchedDef :: SchedDef,
                                    lgPayment :: CTP.Payment,
                                    lgRateConv :: Maybe RateConv, 
                                    lgRounding :: Maybe CT.RoundingRule,
                                    lgStubPerDetail :: ConfStubPerDetail,
                                    lgDayCount :: DayCount,
                                    lgIniExchange :: Bool,
                                    lgIntermPayments :: Bool,
                                    lgFinExchange :: Bool,
                                    lgAccrualConv :: AccrualConv,
                                    lgYieldConv :: YieldConv
                                } 
                  | FloatingLegGen {
                                       lgPayReceive :: CTP.PayReceive, 
                                       lgIRIndex :: IRIndex, 
                                       lgCurrency :: Currency, 
                                       lgStartDelay :: Maybe DateShifter,
                                       lgPayCalendar :: Maybe Calendar,
                                       lgFixCalendar :: Maybe Calendar,
                                       lgSchedDef :: SchedDef,
                                       lgFixing :: CTP.Fixing,
                                       lgPayment :: CTP.Payment,
                                       lgRateConv :: Maybe RateConv, 
                                       lgRounding :: Maybe CT.RoundingRule,
                                       lgStubPerDetail :: ConfStubPerDetail,
                                       lgMarginMode :: MarginMode,
                                       lgDayCount :: DayCount,
                                       lgIniExchange :: Bool,
                                       lgIntermPayments :: Bool,
                                       lgFinExchange :: Bool,
                                       lgAccrualConv :: AccrualConv,
                                       lgYieldConv :: YieldConv
                                   } 
                    deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data SchedDef = FixedSchedDef {
                                  sdCalcStartSchedule :: Maybe CTP.Schedule,
                                  sdCalcEndSchedule   :: Maybe CTP.Schedule,
                                  sdPaymentSchedule   :: Maybe CTP.Schedule,
                                  sdPayFreqRatio      :: Maybe Int,
                                  sdCapitalSchedule   :: Maybe CTP.Schedule
                              }
              | FloatSchedDef {
                                  sdCalcStartSchedule :: Maybe CTP.Schedule,
                                  sdCalcEndSchedule   :: Maybe CTP.Schedule,
                                  sdPaymentSchedule   :: Maybe CTP.Schedule,
                                  sdPayFreqRatio      :: Maybe Int,
                                  sdFixingSchedule    :: Maybe CTP.Schedule,
                                  sdFixFreqRatio      :: Maybe Int,
                                  sdCapitalSchedule   :: Maybe CTP.Schedule
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
                                   acMethod :: String,
                                   acRounding :: String,
                                   acRoundMode :: String
                               }      
                   deriving (Eq, Show, Data, Typeable)       
--------------------------------------------------------------------------
data YieldConv = YieldConv {
                               ycCalculation :: String,
                               ycCoumponding :: String,
                               ycGrossToCleanAccrual :: String,
                               ycAltYieldConvention :: String
                           }      
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
                     swDefaultAmort :: DefaultAmortizing,
                     --swFutCashProccedCutOff :: FurCashProcCutOff,
                     swMarketQuote :: ConfMarketQuote,
                     swAddFlows :: Maybe [AdFlow],
                     swLegs :: [Leg (forall mod. mod)]
                 } deriving (Eq, Show, Data, Typeable)          
--------------------------------------------------------------------------
data AdFlow = AdFlow {
                         flDate :: Day,
                         flQuantity :: CTP.Nominal
                     } deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------
data Leg mod = FixedLeg {
                            lPayReceive :: CTP.PayReceive, 
                            lCurrency :: Currency, 
                            lStartDelay :: DateShifter,
                            lPayCalendar :: Calendar,
                            lSchedDef :: SchedDef,
                            lPayment :: CTP.Payment,
                            lRateConv :: RateConv, 
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
                               lIRIndex :: IRIndex, 
                               lFactor :: Int, 
                               lCurrency :: Currency,
                               lFirstFix :: Double,
                               lRateConv :: RateConv, 
                               lMargin :: Double,
                               lPayCalendar :: Calendar,
                               lFixCalendar :: Calendar,
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
                               lValuationInfo :: Engine mod,
                               lFlows :: [Flow]
                           } deriving (Eq, Show, Data, Typeable)         
-------------------------------------------------------------------------- 
data SwapMktData = EstCurve | DiscCurve | CapFloorVol | SwaptionVol
                   deriving (Eq, Ord, Show, Data, Typeable)         
--------------------------------------------------------------------------

data Flow = FixedFlow {
                           ffCalcStartDate :: Day,
                           ffCalcEndDate :: Day,
                           ffRemCapital :: Double,
                           ffRate :: Double,
                           ffPayDate :: Day,
                           ffFlow :: Double,
                           ffCurr :: Currency
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
                             flfCurr :: Currency
                         } deriving (Eq, Show, Data, Typeable)             
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------



--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------



