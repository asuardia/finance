{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


module Configuration.Forex.Currencies   
    ( 
     Currency, eur, usd
    ) where

import Data.Ratio
import Utils.MyJSON

data Currency = Currency {
                             fullName        :: String, 
                             symbol          :: Maybe String,
                             isoCode         :: String,
                             area            :: Maybe String,
                             rateEntryMode   :: Maybe String,
                             flRateRef       :: Maybe String,
                             shortTermRtConv :: Maybe Convention,
                             longTermRtConv  :: Maybe Convention,
                             longTermSched   :: Maybe Schedule,
                             spotSchedule    :: Maybe Schedule,
                             calendar        :: Calendar,
                             precision       :: Maybe Rational,
                             roundingRule    :: Maybe RoundRule,
                             rate            :: Maybe Rational
                         } deriving (Eq, Show, Data, Typeable, Read)



data RoundRule = RoundRule deriving (Eq, Show, Data, Typeable, Read)
data Schedule = Schedule deriving (Eq, Show, Data, Typeable, Read)
sched = Schedule
data Convention = Convention deriving (Eq, Show, Data, Typeable, Read)
conv = Convention
data Calendar = Calendar deriving (Eq, Show, Data, Typeable, Read)
cal = Calendar

eur = Currency {
                   fullName        = "EURO", 
                   symbol          = Nothing,
                   isoCode         = "EUR",
                   area            = Nothing,
                   rateEntryMode   = Just "Rate",
                   flRateRef       = Just "EURIBOR",
                   shortTermRtConv = Just conv,
                   longTermRtConv  = Just conv,
                   longTermSched   = Just sched,
                   spotSchedule    = Just sched,
                   calendar        = cal,
                   precision       = Just (1 % 100),
                   roundingRule    = Nothing,
                   rate            = Just (7 % 4)
               } 

usd = Currency {
                   fullName        = "US DOLLAR", 
                   symbol          = Just "$",
                   isoCode         = "USD",
                   area            = Nothing,
                   rateEntryMode   = Just "Rate",
                   flRateRef       = Just "EURIBOR",
                   shortTermRtConv = Just conv,
                   longTermRtConv  = Just conv,
                   longTermSched   = Just sched,
                   spotSchedule    = Just sched,
                   calendar        = cal,
                   precision       = Just (1 % 100),
                   roundingRule    = Nothing,
                   rate            = Just (7 % 4)
               } 


