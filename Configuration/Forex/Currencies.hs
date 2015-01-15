{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.Forex.Currencies   
    ( 
     Currency, eur, usd
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
import Configuration.Indices.ArchivingGroups
import qualified Configuration.CommonTypes.Types as CT

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data Currency = Currency {
                             fullName        :: String, 
                             symbol          :: Maybe String,
                             isoCode         :: String,
                             area            :: Maybe String,
                             rateEntryMode   :: RateEntryMode,
                             flRateRef       :: ArchivingGroup ,
                             shortTermRtConv :: RateConv,
                             longTermRtConv  :: RateConv,
                             longTermSched   :: ScheduleGen,
                             spotSchedule    :: DateShifter,
                             calendar        :: Calendar,
                             precision       :: Maybe Rational,
                             roundingRule    :: CT.RoundingRule,
                             rate            :: Maybe Rational
                         } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data RateEntryMode = Rate | SwapPoints deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------

eur = Currency {
                   fullName        = "EURO", 
                   symbol          = Nothing,
                   isoCode         = "EUR",
                   area            = Nothing,
                   rateEntryMode   = Rate,
                   flRateRef       = euriborAGr,
                   shortTermRtConv = lin_act360,
                   longTermRtConv  = lin_30360,
                   longTermSched   = _1Y_MODFOLL,
                   spotSchedule    = plus_2_OPEN_DAYS,
                   calendar        = target,
                   precision       = Just (1 % 100),
                   roundingRule    = CT.None,
                   rate            = Just (7 % 4)
               } 

--------------------------------------------------------------------------
usd = Currency {
                   fullName        = "US DOLLAR", 
                   symbol          = Just "$",
                   isoCode         = "USD",
                   area            = Nothing,
                   rateEntryMode   = Rate,
                   flRateRef       = euriborAGr,
                   shortTermRtConv = lin_act360,
                   longTermRtConv  = lin_30360,
                   longTermSched   = _1Y_MODFOLL,
                   spotSchedule    = plus_2_OPEN_DAYS,
                   calendar        = new_york,
                   precision       = Just (1 % 100),
                   roundingRule    = CT.None,
                   rate            = Just (7 % 4)
               } 


