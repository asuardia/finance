{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Indices.IRIndices   
    ( 
     IRIndex (..), MatUnit(..),
     giveRateDates,
     _EURIBOR3M, _EURIBOR6M
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------

import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars
import Configuration.Forex.Currencies
import Configuration.MktConventions.ScheduleGen
import Configuration.MktConventions.DateShifters
import Configuration.MktConventions.RateConv
import qualified Configuration.CommonTypes.Types as CT
import qualified Configuration.CommonTypes.TypesProducts as CTP
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data IRIndex = IRIndex {
                           iriClassification :: Classification,
                           iriFixings        :: Fixings
                       } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Classification = Classification {
                                         clCategory    :: Category,
                                         clDescription :: Maybe String,
                                         clCode        :: Maybe String,
                                         clIndexDef    :: IndexDef
                                     } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data IndexDef = IndexDef {
                             idNature            :: RateNature,
                             idCurrency          :: Currency,
                             idStartDelay        :: DateShifter,
                             idCalcStartSchedule :: CTP.Schedule,
                             idCalcEndSchedule   :: CTP.Schedule,
                             idPaymentSchedule   :: CTP.Schedule,
                             idFixingSchedule    :: CTP.Schedule,
                             idRateConvention    :: RateConv,
                             idRoundRule         :: CT.RoundingRule,
                             idRateCurve         :: Maybe String,
                             idContango          :: Bool,
                             idEstimationCalends :: Maybe Calendar
                         } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data RateNature = StandardRate
                | SwapRate{
                              rnGenerator :: String,
                              rnMaturity  :: Maturity  
                          } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Maturity = Maturity {
                             matType   :: TypeMaturity,
                             matNumber :: Int,
                             matUnits  :: MatUnit
                         } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------
data TypeMaturity = Tenor deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------
data MatUnit = Year | OtherUnit deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------

data Fixings = Fixings {
                           fixFormulaType    :: FormulaType,
                           fixArchivingGroup :: ArchivingGroup
                       } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Category = Rate   | EquityPrice | BondPrice | Inflation 
              | FxSpot | PoolFactor  | GenericIndex 
              | Formula {
                            catFormulaType :: FormulaType
                        } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data FormulaType = Published | Compounded | Average 
                 | Basket | StartEnd deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ArchivingGroup = ArchivingGroup {archCal :: Calendar} 
                      deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data FixOrStart = Fixing | StartDate deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------

giveRateDates :: FixOrStart -> IRIndex -> Day 
              -> Result_ (Day, Day, Day, Day)
          ---------------------------------
giveRateDates Fixing
              iri@IRIndex {
                              iriClassification = Classification {
                                                                     clIndexDef = IndexDef {
                                                                                               idStartDelay        = dtShift,
                                                                                               idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = stSched},
                                                                                               idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                               idPaymentSchedule   = paySch,
                                                                                               idFixingSchedule    = fixSch
                                                                                           }
                                                                 }
                          }    
              dt = do
    let cal = getCalendar iri
    let fix = checkingCal (sgCalendCheck stSched) (Just cal) (sgRollConv stSched) dt 
    startDay      <- shiftDate dtShift (Just cal) fix
    (endDay' : _) <- genSchedule stSched (Just cal) fix (addGregorianYearsClip 10 fix)
    endDay        <- shiftDate dtShift (Just cal) endDay'
    payDay        <- deduceDay paySch (Just cal) endDay
    return (fix, startDay, endDay, payDay)     
          ---------------------------------
giveRateDates StartDate
              iri@IRIndex {
                              iriClassification = Classification {
                                                                     clIndexDef = IndexDef {
                                                                                               idStartDelay        = dtShift,
                                                                                               idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = stSched},
                                                                                               idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                               idPaymentSchedule   = paySch,
                                                                                               idFixingSchedule    = fixSch
                                                                                           }
                                                                 }
                          }    
              dt = do
    let cal = getCalendar iri
    let startDay = checkingCal (sgCalendCheck stSched) (Just cal) (sgRollConv stSched) dt 
    fixDay        <- deduceDay fixSch (Just cal) startDay
    --let iniDay = if (sgGeneration stSched == Normal) then dt else startDay
    --let iniDay = startDay
    (endDay' : _) <- genSchedule stSched (Just cal) fixDay (addGregorianYearsClip 10 fixDay)
    endDay        <- shiftDate dtShift (Just cal) endDay'
    payDay        <- deduceDay paySch (Just cal) endDay
    return (fixDay, startDay, endDay, payDay)      
          ---------------------------------

giveRateDates _ _ _ = Error_ " giveRateDates: option not implemented "
--------------------------------------------------------------------------
getCalendar :: IRIndex -> Calendar
getCalendar IRIndex {
                        iriClassification = Classification {
                                                               clIndexDef = IndexDef {
                                                                                         idEstimationCalends = Nothing
                                                                                     }
                                                           },
                        iriFixings = Fixings {
                                                 fixArchivingGroup = archGroup
                                             }
                    } = archCal archGroup     
          ---------------------------------
getCalendar IRIndex {
                        iriClassification = Classification {
                                                               clIndexDef = IndexDef {
                                                                                         idEstimationCalends = Just cal
                                                                                     }
                                                           }
                    } = cal
--------------------------------------------------------------------------
deduceDay :: CTP.Schedule -> Maybe Calendar -> Day -> Result_ Day
deduceDay CTP.SchEqual2{} mbCal dt = Ok_ dt
deduceDay CTP.SchDeducedFrom {
                                CTP.schDedForm = shift                                
                            } 
          mbCal dt = shiftDate shift mbCal dt
deduceDay _ _ _ = Error_ " deduceDay: option not implemented "

--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------

iriEx1 = giveRateDates Fixing _EURIBOR3M (fromGregorian 2014 12 30)
iriEx2 = giveRateDates StartDate _EURIBOR3M (fromGregorian 2015 1 1)
iriEx3 = giveRateDates StartDate _EURCMS6Y (fromGregorian 2015 1 1)
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
euriborAGr = ArchivingGroup {archCal = target}
costMatSwapAGr = ArchivingGroup {archCal = target}
--------------------------------------------------------------------------
_EURIBOR3M = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = StandardRate,
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = plus_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = _3M_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchDeducedFrom {
                                                                                                                                        CTP.schDedFrom = CTP.SchStart,
                                                                                                                                        CTP.schDedForm = minus_2_OPEN_DAYS
                                                                                                                                    },
                                                                                            idRateConvention    = lin_act360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Nothing
                                                                                        }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = euriborAGr
                                              }
                     }
--------------------------------------------------------------------------
_EURIBOR6M = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = StandardRate,
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = plus_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = _6M_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchDeducedFrom {
                                                                                                                                        CTP.schDedFrom = CTP.SchStart,
                                                                                                                                        CTP.schDedForm = minus_2_OPEN_DAYS
                                                                                                                                    },
                                                                                            idRateConvention    = lin_act360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Nothing
                                                                                       }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = euriborAGr
                                              }
                     }
--------------------------------------------------------------------------
_EURCMS6Y = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = SwapRate{
                                                                                                                              rnGenerator = "", --SwapGenerator _EURIBOR6M,
                                                                                                                              rnMaturity  = Maturity {
                                                                                                                                                         matType   = Tenor,
                                                                                                                                                         matNumber = 6,
                                                                                                                                                         matUnits  = Year
                                                                                                                                                     }  
                                                                                                                          },
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = plus_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = _1Y_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchEqual2 {CTP.equal2 = CTP.SchPayment},
                                                                                            idRateConvention    = lin_act360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Nothing
                                                                                       }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = costMatSwapAGr
                                              }
                     }

