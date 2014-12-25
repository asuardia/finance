{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.MktConventions.ScheduleGen
    ( 
     ScheduleGen (..), shiftDate',
     _6M_MODFOLL
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

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data ScheduleGen = SimpleGen {
                                 sgCalendCheck  :: DSh.CalendarCheck,
                                 sgKeepIdDates  :: Bool,
                                 sgGenFrequency :: GenerationFreq,
                                 sgUnit         :: Units,
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
                                   sgUnit        :: Units,
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
data Units = Month	 
           | Year deriving (Eq, Show, Data, Typeable, Read)
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
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
       where dt1'  = shiftDate' units e2e dt1 n
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
       where dt2'  = shiftDate' units e2e dt2 (-n)
             dt2'' = checkingCal calCheck mbCal rollConv dt2'
            
          ---------------------------------
genSchedule _ _ _ _ = Error_ " genSchedule: option not implemented "         
--------------------------------------------------------------------------
shiftDate' :: Units -> Bool -> Day -> Int -> Day
shiftDate' Month True  dt n = pass2EOM $  
    addGregorianMonthsClip (toInteger n) dt
shiftDate' Month False dt n = addGregorianMonthsRollOver  (toInteger n) dt
shiftDate' Year  True  dt n = pass2EOM $
    addGregorianMonthsClip (toInteger $ n*12) dt
shiftDate' Year  False dt n = addGregorianMonthsRollOver  (toInteger $ n*12) dt

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
_6M_MODFOLL = SimpleGen {
                            sgCalendCheck  = DSh.External,
                            sgKeepIdDates  = False,
                            sgGenFrequency = Forward,
                            sgUnit         = Month,
                            sgNumber       = 6,
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
sgEx1 = genSchedule _6M_MODFOLL (Just target) (fromGregorian 2014 2 28) (fromGregorian 2019 3 6) 

