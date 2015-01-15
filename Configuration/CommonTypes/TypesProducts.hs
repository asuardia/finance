{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.CommonTypes.TypesProducts   
    ( 
     Schedule (..), SchOption (..), Payment (..), Frequency (..),
     Fixing (..), PayReceive (..), Maturity (..), Tenor (..), Unit (..),
     Nominal (..)
     
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Utils.MyJSON
import Configuration.Forex.Currencies
import Configuration.MktConventions.DateShifters
import Configuration.MktConventions.ScheduleGen

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data Schedule = DrivingSchedule {
                                    schedule :: ScheduleGen
                                }
              | SchEqual2 { equal2 :: SchOption} 
              | SchDeducedFrom {
                                        schDedFrom :: SchOption,
                                        schDedForm :: DateShifter
                               } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data SchOption = SchStart | SchPayment 
                 deriving (Eq, Show, Data, Typeable)             
--------------------------------------------------------------------------
data Payment = InArrearsP | UpFrontP | UpFrontDiscP
               deriving (Eq, Show, Data, Typeable)           
--------------------------------------------------------------------------
data Fixing = InArrearsFix | UpFrontFix
              deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data PayReceive = Pay | Receive
                  deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Maturity = Maturity {
                             matTenor :: Maybe Tenor,
                             matDate :: Maybe Day
                         }
                deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Tenor = Tenor {
                       tenorUnit :: Unit,
                       tenorQuantity :: Int
                   } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Unit = Year | Month
            deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Frequency = Annually | SemiAnnually | Quarterly | Monthly
                deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Nominal = Nominal {
                             nomQuantity :: Integer,
                             nomCurrency :: Currency
                       }
               deriving (Eq, Show, Data, Typeable)
                 