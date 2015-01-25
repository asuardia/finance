{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.MktConventions.ScheduleGen
    ( 
     ScheduleGen (..), ScheduleGenLabel, Generation (..), GenerationFreq (..),
     idScheduleGen, shiftDate', genSchedule, 
     sg3M_MODFOLL, sg6M_MODFOLL, sg1Y_MODFOLL,
     _3m_modfoll, _6m_modfoll, _1y_modfoll
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.List (sort)
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars as Cal
import Configuration.MktConventions.DateAdjustments as DAdj
import Configuration.MktConventions.DateShifters as DSh
import qualified Configuration.CommonTypes.Types as T


--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type ScheduleGenLabel = String
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data ScheduleGen = SimpleGen {
                                 sgCalendCheck  :: DSh.CalendarCheck,
                                 sgKeepIdDates  :: Bool,
                                 sgGenFrequency :: GenerationFreq,
                                 sgUnit         :: T.Unit,
                                 sgNumber       :: Int,
                                 sgEndToEnd     :: Bool,
                                 sgGeneration   :: Generation,
                                 sgRollConv     :: DSh.RollConvention,
                                 sgAdjustment   :: Maybe DAdj.DateAdjustment,
                                 sgStartDate    :: Maybe ShiftDef,
                                 sgEndDate      :: Maybe DAdj.DateAdjustment
                               } 
                 | RelativeGen {
                                   sgRelativeGen :: ScheduleGen,
                                   sgCalendCheck :: DSh.CalendarCheck,
                                   sgAdjBefShift :: Maybe DAdj.DateAdjustment,
                                   sgKeepIdDates :: Bool,
                                   sgShiftDef    :: ShiftDef,
                                   sgUnit        :: T.Unit,
                                   sgNumber      :: Int,
                                   sgEndToEnd    :: Bool,
                                   sgGeneration  :: Generation,
                                   sgRollConv    :: DSh.RollConvention,
                                   sgAdjAftShift :: Maybe DAdj.DateAdjustment
                               } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
                               
data GenerationFreq = Forward | Backward deriving (Eq, Show, Data, Typeable)
type ShiftDef = GenerationFreq
--------------------------------------------------------------------------
data Generation = Normal	 
                | Recursive deriving (Eq, Show, Data, Typeable, Read)
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
idScheduleGen :: ScheduleGenLabel -> Result_ ScheduleGen
idScheduleGen "3M_MODFOLL"   = Ok_ _3m_modfoll
idScheduleGen "6M_MODFOLL" = Ok_ _6m_modfoll
idScheduleGen "1Y_MODFOLL" = Ok_ _1y_modfoll
idScheduleGen sg = Error_ (" idScheduleGen: " ++ sg ++ "Not identified schedule generator. ")
--------------------------------------------------------------------------
genSchedule :: ScheduleGen -> Maybe Cal.Calendar -> Day -> Day 
            -> Result_ [Day]
          ---------------------------------
genSchedule SimpleGen {
                          sgCalendCheck  = calCheck,
                          sgGenFrequency = Forward,
                          sgUnit         = units,
                          sgNumber       = n,
                          sgEndToEnd     = e2e,
                          sgGeneration   = Normal,
                          sgRollConv     = rollConv,
                          sgAdjustment   = Nothing,
                          sgStartDate    = Nothing,
                          sgEndDate      = Nothing
                      }
            mbCal dt1 dt2 = Ok_ dts2
            where dts  = fmap (shiftDate' units e2e dt1) [n*ls | ls <- [1..]]
                  dts2 = takeWhile ((>) dt2) 
                         $ fmap (checkingCal calCheck mbCal rollConv) dts
          ---------------------------------
genSchedule SimpleGen {
                          sgCalendCheck  = calCheck,
                          sgGenFrequency = Backward,
                          sgUnit         = units,
                          sgNumber       = n,
                          sgEndToEnd     = e2e,
                          sgGeneration   = Normal,
                          sgRollConv     = rollConv,
                          sgAdjustment   = Nothing,
                          sgStartDate    = Nothing,
                          sgEndDate      = Nothing
                      }
            mbCal dt1 dt2 = Ok_ (sort dts2)
            where dts  = fmap (shiftDate' units e2e dt2) [n*ls | ls <- [-1,-2..]]
                  dts2 = takeWhile ((<) dt1)
                         $ fmap (checkingCal calCheck mbCal rollConv) dts
          ---------------------------------
genSchedule sg@SimpleGen {
                         sgCalendCheck  = calCheck,
                         sgGenFrequency = Forward,
                         sgUnit         = units,
                         sgNumber       = n,
                         sgEndToEnd     = e2e,
                         sgGeneration   = Recursive,
                         sgRollConv     = rollConv,
                         sgAdjustment   = Nothing,
                         sgStartDate    = Nothing,
                         sgEndDate      = Nothing
                       }
            mbCal dt1 dt2 
                | dt2 <= dt1'' = Ok_ [] 
                | otherwise = 
       Ok_ (dt1'' : ((\(Ok_ x) -> x) $ genSchedule sg mbCal dt1'' dt2))
       where dt1'  = shiftDate' units e2e (checkingCal calCheck mbCal rollConv dt1) n
             dt1'' = checkingCal calCheck mbCal rollConv dt1'
          ---------------------------------
genSchedule sg@SimpleGen {
                         sgCalendCheck  = calCheck,
                         sgGenFrequency = Backward,
                         sgUnit         = units,
                         sgNumber       = n,
                         sgEndToEnd     = e2e,
                         sgGeneration   = Recursive,
                         sgRollConv     = rollConv,
                         sgAdjustment   = Nothing,
                         sgStartDate    = Nothing,
                         sgEndDate      = Nothing
                       }
            mbCal dt1 dt2 
                | dt2'' <= dt1 = Ok_ [] 
                | otherwise = 
       Ok_ (sort (dt2'' : ((\(Ok_ x) -> x) $ genSchedule sg mbCal dt1 dt2'')))
       where dt2'  = shiftDate' units e2e (checkingCal calCheck mbCal rollConv dt2) (-n)
             dt2'' = checkingCal calCheck mbCal rollConv dt2'
            
          ---------------------------------
genSchedule _ _ _ _ = Error_ " genSchedule: option not implemented "         
--------------------------------------------------------------------------
shiftDate' :: T.Unit -> Bool -> Day -> Int -> Day
shiftDate' T.Month True  dt n = pass2EOM $  
    addGregorianMonthsClip (toInteger n) dt
shiftDate' T.Month False dt n = addGregorianMonthsClip  (toInteger n) dt
shiftDate' T.Year  True  dt n = pass2EOM $
    addGregorianMonthsClip (toInteger $ n*12) dt
shiftDate' T.Year  False dt n = addGregorianMonthsClip  (toInteger $ n*12) dt

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
sg3M_MODFOLL = "3M_MODFOLL"
_3m_modfoll = SimpleGen {
                            sgCalendCheck  = DSh.External,
                            sgKeepIdDates  = False,
                            sgGenFrequency = Forward,
                            sgUnit         = T.Month,
                            sgNumber       = 3,
                            sgEndToEnd     = False,
                            sgGeneration   = Normal,
                            sgRollConv     = ModFoll, 
                            sgAdjustment   = Nothing,
                            sgStartDate    = Nothing,
                            sgEndDate      = Nothing
                        } 
--------------------------------------------------------------------------
sg6M_MODFOLL = "6M_MODFOLL"
_6m_modfoll = SimpleGen {
                            sgCalendCheck  = DSh.External,
                            sgKeepIdDates  = False,
                            sgGenFrequency = Forward,
                            sgUnit         = T.Month,
                            sgNumber       = 6,
                            sgEndToEnd     = False,
                            sgGeneration   = Normal,
                            sgRollConv     = ModFoll, 
                            sgAdjustment   = Nothing,
                            sgStartDate    = Nothing,
                            sgEndDate      = Nothing
                        } 
--------------------------------------------------------------------------
sg1Y_MODFOLL = "1Y_MODFOLL"
_1y_modfoll = SimpleGen {
                            sgCalendCheck  = DSh.External,
                            sgKeepIdDates  = False,
                            sgGenFrequency = Forward,
                            sgUnit         = T.Month,
                            sgNumber       = 12,
                            sgEndToEnd     = False,
                            sgGeneration   = Normal,
                            sgRollConv     = ModFoll, 
                            sgAdjustment   = Nothing,
                            sgStartDate    = Nothing,
                            sgEndDate      = Nothing
                        } 


--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------
sgEx1 = genSchedule _6m_modfoll (Just target) (fromGregorian 2014 2 28) (fromGregorian 2019 3 6) 

