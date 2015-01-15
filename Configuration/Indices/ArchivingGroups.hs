{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.Indices.ArchivingGroups   
    ( 
     ArchivingGroup (..), euriborAGr, costMatSwapAGr
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Ratio
import Utils.MyJSON
import Configuration.MktConventions.Calendars
import Configuration.MktConventions.ScheduleGen
import Configuration.MktConventions.RateConv
import Configuration.MktConventions.DateShifters
import qualified Configuration.CommonTypes.Types as CT

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
data ArchivingGroup = ArchivingGroup {
                                         agFixingCal :: Calendar,
                                         agFixingFreq :: Frequency,
                                         agRoundRule :: CT.RoundingRule,
                                         agDecimals :: Int                                         
                                     } 
                      deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Frequency = Daily | Weekly | Monthly deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------

euriborAGr = ArchivingGroup {
                                 agFixingCal = target,
                                 agFixingFreq = Daily,
                                 agRoundRule = CT.Nearest,
                                 agDecimals = 3                                         
                             } 
--------------------------------------------------------------------------
costMatSwapAGr = ArchivingGroup {
                                    agFixingCal = target,
                                    agFixingFreq = Daily,
                                    agRoundRule = CT.Nearest,
                                    agDecimals = 3                                         
                                } 
--------------------------------------------------------------------------