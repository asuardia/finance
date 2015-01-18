{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Indices.IRIndices   
    ( 
     IRIndex (..), IRIndexLabel,
     giveRateDates,
     euribor3m, euribor6m, eurcms6y, 
     iEURIBOR3M, iEURIBOR6M, iEURCMS6Y
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
import Configuration.Indices.ArchivingGroups
import qualified Configuration.CommonTypes.Types as CT
import qualified Configuration.CommonTypes.TypesProducts as CTP
--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type IRIndexLabel = String
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
                             idStartDelay        :: DateShifterLabel,
                             idCalcStartSchedule :: CTP.Schedule,
                             idCalcEndSchedule   :: CTP.Schedule,
                             idPaymentSchedule   :: CTP.Schedule,
                             idFixingSchedule    :: CTP.Schedule,
                             idRateConvention    :: RateConvLabel,
                             idRoundRule         :: CT.RoundingRule,
                             idRateCurve         :: Maybe String,
                             idContango          :: Bool,
                             idEstimationCalends :: EstimationCalendars
                         } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data RateNature = StandardRate
                | SwapRate{
                              rnGenerator :: String,
                              rnMaturity  :: CTP.Maturity  
                          } deriving (Eq, Show, Data, Typeable)
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
data FixOrStart = Fixing | StartDate deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data EstimationCalendars = Inherited 
                         | Redfined { 
                                        starDatesCal :: CalendarType,
                                        indexDatesCal :: CalendarType
                                    } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data CalendarType = Archiving | PayCurrency 
                  | ArchPlusPayCurr deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
idIRIndex :: IRIndexLabel -> Result_ IRIndex
idIRIndex "EURIBOR3M"   = Ok_ euribor3m
idIRIndex "EURIBOR6M"   = Ok_ euribor6m
idIRIndex "EURCMS6Y" = Ok_ eurcms6y
idIRIndex i = Error_ (" idIRIndex: " ++ i ++ ". Not identified index. ")
--------------------------------------------------------------------------

giveRateDates :: FixOrStart -> IRIndex -> Day 
              -> Result_ (Day, Day, Day, Day)
          ---------------------------------
giveRateDates Fixing
              iri@IRIndex {
                              iriClassification = Classification {
                                                                     clIndexDef = IndexDef {
                                                                                               idStartDelay        = dtShiftLab,
                                                                                               idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = stSchedLab},
                                                                                               idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                               idPaymentSchedule   = paySch,
                                                                                               idFixingSchedule    = fixSch
                                                                                           }
                                                                 }
                          }    
              dt = do 
    dtShift <- idDateShifter dtShiftLab
    stSched <- idScheduleGen stSchedLab
    let cal = getCalendar iri
    let fix = checkingCal (sgCalendCheck stSched) (Just cal) (sgRollConv stSched) dt
    startDay      <- shiftDate dtShift (Just cal) fix
    (endDay : _) <- genSchedule stSched (Just cal) fix (addGregorianYearsClip 10 fix)
    --endDay        <- shiftDate dtShift (Just cal) endDay'
    payDay        <- deduceDay paySch (Just cal) endDay
    return (fix, startDay, endDay, payDay)     
          ---------------------------------
giveRateDates StartDate
              iri@IRIndex {
                              iriClassification = Classification {
                                                                     clIndexDef = IndexDef {
                                                                                               idStartDelay        = dtShiftLab,
                                                                                               idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = stSchedLab},
                                                                                               idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                               idPaymentSchedule   = paySch,
                                                                                               idFixingSchedule    = fixSch
                                                                                           }
                                                                 }
                          }    
              dt = do
    dtShift <- idDateShifter dtShiftLab
    stSched <- idScheduleGen stSchedLab
    let cal = getCalendar iri
    let startDay = checkingCal (sgCalendCheck stSched) (Just cal) (sgRollConv stSched) dt 
    fixDay        <- deduceDay fixSch (Just cal) startDay
    --let iniDay = if (sgGeneration stSched == Normal) then dt else startDay
    --let iniDay = startDay
    (endDay : _) <- genSchedule stSched (Just cal) dt (addGregorianYearsClip 10 dt)
    payDay        <- deduceDay paySch (Just cal) endDay
    return (fixDay, startDay, endDay, payDay)      
          ---------------------------------

giveRateDates _ _ _ = Error_ " giveRateDates: option not implemented "
--------------------------------------------------------------------------
getCalendar :: IRIndex -> Calendar
getCalendar IRIndex {
                        iriClassification = Classification {
                                                               clIndexDef = IndexDef {
                                                                                         idEstimationCalends = Inherited
                                                                                     }
                                                           },
                        iriFixings = Fixings {
                                                 fixArchivingGroup = archGroup
                                             }
                    } = agFixingCal archGroup     
          ---------------------------------
getCalendar IRIndex {
                        iriClassification = Classification {
                                                               clIndexDef = IndexDef {
                                                                                         idEstimationCalends = _
                                                                                     }
                                                           },
                        iriFixings = Fixings {
                                                 fixArchivingGroup = archGroup
                                             }
                    } = agFixingCal archGroup  
--------------------------------------------------------------------------
deduceDay :: CTP.Schedule -> Maybe Calendar -> Day -> Result_ Day
deduceDay CTP.SchEqual2{} mbCal dt = Ok_ dt
deduceDay CTP.SchDeducedFrom {
                                 CTP.schDedForm = shiftLab                                
                             } 
          mbCal dt = do
    shift <- idDateShifter shiftLab
    shiftDate shift mbCal dt
deduceDay _ _ _ = Error_ " deduceDay: option not implemented "

--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------

iriEx1 = giveRateDates Fixing euribor3m (fromGregorian 2014 12 30)
iriEx2 = giveRateDates StartDate euribor3m (fromGregorian 2015 1 1)
iriEx3 = giveRateDates StartDate eurcms6y (fromGregorian 2015 1 1)
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
iEURIBOR3M = "EURIBOR3M"
euribor3m = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = StandardRate,
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = dsPLUS_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = sg3M_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchDeducedFrom {
                                                                                                                                        CTP.schDedFrom = CTP.SchStart,
                                                                                                                                        CTP.schDedForm = dsMINUS_2_OPEN_DAYS
                                                                                                                                    },
                                                                                            idRateConvention    = rcLIN_ACT360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Inherited
                                                                                        }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = euriborAGr
                                              }
                     }
--------------------------------------------------------------------------
iEURIBOR6M = "EURIBOR6M"
euribor6m = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = StandardRate,
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = dsPLUS_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = sg6M_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchDeducedFrom {
                                                                                                                                        CTP.schDedFrom = CTP.SchStart,
                                                                                                                                        CTP.schDedForm = dsMINUS_2_OPEN_DAYS
                                                                                                                                    },
                                                                                            idRateConvention    = rcLIN_ACT360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Inherited
                                                                                       }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = euriborAGr
                                              }
                     }
--------------------------------------------------------------------------
iEURCMS6Y = "EURCMS6Y"
eurcms6y = IRIndex {
                         iriClassification = Classification {
                                                               clCategory    = Rate,
                                                               clDescription = Nothing,
                                                               clCode        = Nothing,
                                                               clIndexDef    = IndexDef {
                                                                                            idNature            = SwapRate{
                                                                                                                              rnGenerator = "", --SwapGenerator euribor6m,
                                                                                                                              rnMaturity  = CTP.Maturity {
                                                                                                                                                             CTP.matTenor = Just CTP.Tenor {
                                                                                                                                                                                              CTP.tenorUnit = CTP.Year,
                                                                                                                                                                                              CTP.tenorQuantity = 6
                                                                                                                                                                                           },
                                                                                                                                                             CTP.matDate = Nothing
                                                                                                                                                          }  
                                                                                                                          },
                                                                                            idCurrency          = eur,
                                                                                            idStartDelay        = dsPLUS_2_OPEN_DAYS,
                                                                                            idCalcStartSchedule = CTP.DrivingSchedule {CTP.schedule = sg1Y_MODFOLL},
                                                                                            idCalcEndSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idPaymentSchedule   = CTP.SchEqual2 {CTP.equal2 = CTP.SchStart},
                                                                                            idFixingSchedule    = CTP.SchEqual2 {CTP.equal2 = CTP.SchPayment},
                                                                                            idRateConvention    = rcLIN_ACT360,
                                                                                            idRoundRule         = CT.None,
                                                                                            idRateCurve         = Nothing,
                                                                                            idContango          = False,
                                                                                            idEstimationCalends = Inherited
                                                                                       }
                                                            },
                         iriFixings = Fixings {
                                                  fixFormulaType    = Published,
                                                  fixArchivingGroup = costMatSwapAGr
                                              }
                     }

