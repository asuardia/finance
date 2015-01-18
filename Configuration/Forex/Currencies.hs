{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.Forex.Currencies   
    ( 
     Currency, eur, usd, cEUR, cUSD, CurrencyLabel,
     identifyCurrency
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Ratio
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars
import Configuration.MktConventions.ScheduleGen
import Configuration.MktConventions.RateConv
import Configuration.MktConventions.DateShifters
import Configuration.Indices.ArchivingGroups
import qualified Configuration.CommonTypes.Types as CT
--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type CurrencyLabel = String

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
                             shortTermRtConv :: RateConvLabel,
                             longTermRtConv  :: RateConvLabel,
                             longTermSched   :: ScheduleGenLabel,
                             spotSchedule    :: DateShifterLabel,
                             calendar        :: CalendarLabel,
                             precision       :: Maybe Rational,
                             roundingRule    :: CT.RoundingRule,
                             rate            :: Maybe Rational
                         } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data RateEntryMode = Rate | SwapPoints deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
identifyCurrency :: CurrencyLabel -> Result_ Currency
identifyCurrency "EUR"   = Ok_ eur
identifyCurrency "USD" = Ok_ usd
identifyCurrency c = Error_ (" idCurrency: " ++ c ++ ". Not identified currency. ")
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
cEUR = "EUR"
eur = Currency {
                   fullName        = "EURO", 
                   symbol          = Nothing,
                   isoCode         = "EUR",
                   area            = Nothing,
                   rateEntryMode   = Rate,
                   flRateRef       = euriborAGr,
                   shortTermRtConv = rcLIN_ACT360,
                   longTermRtConv  = rcLIN_30360,
                   longTermSched   = sg1Y_MODFOLL,
                   spotSchedule    = dsPLUS_2_OPEN_DAYS,
                   calendar        = cTARGET,
                   precision       = Just (1 % 100),
                   roundingRule    = CT.None,
                   rate            = Just (7 % 4)
               } 

--------------------------------------------------------------------------
cUSD = "USD"
usd = Currency {
                   fullName        = "US DOLLAR", 
                   symbol          = Just "$",
                   isoCode         = "USD",
                   area            = Nothing,
                   rateEntryMode   = Rate,
                   flRateRef       = euriborAGr,
                   shortTermRtConv = rcLIN_ACT360,
                   longTermRtConv  = rcLIN_30360,
                   longTermSched   = sg1Y_MODFOLL,
                   spotSchedule    = dsPLUS_2_OPEN_DAYS,
                   calendar        = cNEW_YORK,
                   precision       = Just (1 % 100),
                   roundingRule    = CT.None,
                   rate            = Just (7 % 4)
               } 


