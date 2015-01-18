{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.MktConventions.DateShifters   
    ( 
     DateShifter (..), DateShifterLabel, CalendarCheck (..), 
     RollConvention (..),
     shiftDate, checkingCal, applyRollConv, addBusDays,
     idDateShifter,
     plus_2_bd_eur, plus_2_open_days, minus_2_open_days,
     dsPLUS_2_BD_EUR, dsPLUS_2_OPEN_DAYS, dsMINUS_2_OPEN_DAYS
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars as Cal
import Configuration.MktConventions.DateAdjustments as DAdj
--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type DateShifterLabel = String
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data CalendarCheck = None	 
                   | Weekend	
                   | External 
                   | Internal {checkCal :: Cal.Calendar}  
                   | External_Plus {checkCalI :: Cal.Calendar} 
                   deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data ShiftDef = Forward	 
              | Backward deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data ShiftUnits = Days	 
                | BusDays	
                | Weeks	 
                | Months 
                | Quarters	 
                | Semesters 
                | Years deriving (Eq, Show, Data, Typeable, Read)
--------------------------------------------------------------------------
data RollConvention = Previous	 
                 | Next	
                 | ModFoll	 
                 | Indifferent deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data DateShifter = DateShifter {
                                   relativeShifter :: Maybe DateShifter,
                                   calendarCheck   :: CalendarCheck,
                                   adjBefShift     :: Maybe DAdj.DateAdjustment,
                                   adjAftShift     :: Maybe DAdj.DateAdjustment,
                                   shiftDef        :: ShiftDef,
                                   unit            :: ShiftUnits,
                                   number          :: Integer,
                                   ifNonBD         :: RollConvention,
                                   keepIdDates     :: Bool
                               } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
idDateShifter :: DateShifterLabel -> Result_ DateShifter
idDateShifter "+2_BD_EUR"   = Ok_ plus_2_bd_eur
idDateShifter "+2_OPEN_DAYS" = Ok_ plus_2_open_days
idDateShifter "-2_OPEN_DAYS"   = Ok_ minus_2_open_days
idDateShifter ds = Error_ (" idDateShifter: " ++ ds ++ "Not identified date shifter. ")
--------------------------------------------------------------------------


shiftDate :: DateShifter -> Maybe Calendar -> Day -> Result_ Day
          ---------------------------------
shiftDate DateShifter {
                          relativeShifter = Nothing,
                          calendarCheck   = calCheck,
                          adjBefShift     = Nothing,
                          adjAftShift     = Nothing,
                          shiftDef        = fwbw,
                          unit            = Days,
                          number          = n,
                          ifNonBD         = ifnbd
                      }
          extCal            
          dt  
          | fwbw == Forward  = Ok_ (checkingCal calCheck extCal ifnbd $ addDays n dt)
          | fwbw == Backward = Ok_ (checkingCal calCheck extCal ifnbd $ addDays (- n) dt)
          ---------------------------------
shiftDate DateShifter {
                          relativeShifter = Nothing,
                          calendarCheck   = calCheck,
                          adjBefShift     = Nothing,
                          adjAftShift     = Nothing,
                          shiftDef        = fwbw,
                          unit            = BusDays,
                          number          = n
                      }
          extCal            
          dt  
          | fwbw == Forward  = Ok_ (addBusDays calCheck extCal n dt)
          | fwbw == Backward = Ok_ (addBusDays calCheck extCal (-n) dt)
          ---------------------------------
shiftDate _ _ _ = Error_ " shiftDate: Option not implemented"
            
--------------------------------------------------------------------------
checkingCal :: CalendarCheck -> Maybe Calendar -> RollConvention -> Day -> Day
          ---------------------------------
checkingCal None _ _ dt = dt	 
          ---------------------------------
checkingCal Weekend _ ifNonBD dt = 
    if isWeekEnd dt 
    then let cal = Calendar Nothing Nothing [Saturday,Sunday] Nothing
         in applyRollConv ifNonBD cal dt 
    else dt 	
          ---------------------------------
checkingCal External (Just cal) ifNonBD dt = 
    if isInCalendar cal dt 
    then applyRollConv ifNonBD cal dt 
    else dt 	 
          ---------------------------------
checkingCal (Internal cal) _ ifNonBD dt = 
    if isInCalendar cal dt 
    then applyRollConv ifNonBD cal dt 
    else dt  
          ---------------------------------
checkingCal (External_Plus cal) (Just calExt) ifNonBD dt = 
    let cals = CalendarUnion [cal, calExt] 
    in if isInCalendar cals dt 
       then applyRollConv ifNonBD cals dt 
       else dt      

--------------------------------------------------------------------------
applyRollConv :: RollConvention -> Calendar -> Day -> Day
applyRollConv Previous cal dt = addBusDays External (Just cal) (-1) dt 
applyRollConv Next cal dt     = addBusDays External (Just cal) 1 dt  
applyRollConv ModFoll cal dt  = let next = addBusDays External (Just cal) 1 dt
                                in if (snd3 $ toGregorian next) /=
                                    (snd3 $ toGregorian dt)
                                   then addBusDays External (Just cal) (-1) dt
                                   else next 
applyRollConv Indifferent cal dt = dt 
--------------------------------------------------------------------------
addBusDays :: CalendarCheck -> Maybe Calendar -> Integer -> Day -> Day
          ---------------------------------
addBusDays None _ n dt1 = addDays n dt1
          ---------------------------------
addBusDays _    _ 0 dt1 = dt1 
          ---------------------------------
addBusDays Weekend mc n dt1 = 
    let unit = sign (fromIntegral n :: Double)
        dt2 = addBusDays None mc unit dt1
        n' =  n - unit
    in if isWeekEnd dt2
    then addBusDays Weekend mc n dt2
    else addBusDays Weekend mc n' dt2
          ---------------------------------
addBusDays External (Just cal) n dt1 = 
    let unit = sign (fromIntegral n :: Double)
        dt2 = addBusDays None Nothing unit dt1
        n' =  n - unit
    in if isInCalendar cal dt2
    then addBusDays External (Just cal) n dt2
    else addBusDays External (Just cal) n' dt2
          ---------------------------------
addBusDays (Internal cal) mc n dt1 = 
    let unit = sign (fromIntegral n :: Double)
        dt2 = addBusDays None Nothing unit dt1
        n' =  n - unit
    in if isInCalendar cal dt2
    then addBusDays (Internal cal) mc n dt2
    else addBusDays (Internal cal) mc n' dt2
          ---------------------------------
addBusDays (External_Plus cal) (Just calExt) n dt1 = 
    let unit = sign (fromIntegral n :: Double)
        dt2 = addBusDays None Nothing unit dt1
        n' =  n - unit
        unionCal = CalendarUnion [cal, calExt]
    in if isInCalendar unionCal dt2
    then addBusDays (External_Plus cal) (Just calExt) n dt2
    else addBusDays (External_Plus cal) (Just calExt) n' dt2

--------------------------------------------------------------------------
buildCalendar :: CalendarCheck -> Maybe Cal.Calendar -> Cal.Calendar
buildCalendar Weekend _ = Calendar {
                                       description = Nothing,
                                       swiftCode = Nothing,
                                       weekEnd = [Cal.Saturday, Cal.Sunday],
                                       holidays = Nothing 
                                   }
buildCalendar External (Just cal) = cal
buildCalendar (Internal cal) _    = cal
buildCalendar (External_Plus cal) (Just calExt) = CalendarUnion [cal, calExt]
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------

dsPLUS_2_BD_EUR = "+2_BD_EUR"
plus_2_bd_eur = DateShifter {
                                relativeShifter = Nothing,
                                calendarCheck   = Internal Cal.target,
                                adjBefShift     = Nothing,
                                adjAftShift     = Nothing,
                                shiftDef        = Forward,
                                unit            = BusDays,
                                number          = 2,
                                ifNonBD         = Next,
                                keepIdDates     = False
                            } 
--------------------------------------------------------------------------
dsPLUS_2_OPEN_DAYS = "+2_OPEN_DAYS"
plus_2_open_days = DateShifter {
                                   relativeShifter = Nothing,
                                   calendarCheck   = External,
                                   adjBefShift     = Nothing,
                                   adjAftShift     = Nothing,
                                   shiftDef        = Forward,
                                   unit            = BusDays,
                                   number          = 2,
                                   ifNonBD         = Next,
                                   keepIdDates     = False
                              } 
--------------------------------------------------------------------------
dsMINUS_2_OPEN_DAYS = "-2_OPEN_DAYS"
minus_2_open_days = DateShifter {
                                    relativeShifter = Nothing,
                                    calendarCheck   = External,
                                    adjBefShift     = Nothing,
                                    adjAftShift     = Nothing,
                                    shiftDef        = Backward,
                                    unit            = BusDays,
                                    number          = 2, 
                                    ifNonBD         = Next,
                                    keepIdDates     = False
                               } 



