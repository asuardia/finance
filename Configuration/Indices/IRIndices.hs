{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Indices.IRIndices   
    ( 
     
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars as Cal
import Configuration.MktConventions.DateAdjustments as DAdj
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data IRIndex = IRIndex {
                           iriClassification :: Classification,
                           iriFixings :: Fixings
                       } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Classification = Classification {
                                         clCategory :: Category,
                                         clDescription :: Maybe String,
                                         clCode :: Maybe String,
                                         clIndexDef :: IndexDef
                                     } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Fixings = Fixings {
                           fixFormulaType :: FormulaType,
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
data ArchivingGroup = EuriborAGr deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------


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
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------




