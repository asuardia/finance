{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Configuration.CommonTypes.TypesProducts   
    ( 
     Schedule (..), SchOption (..), Payment (..), Frequency (..),
     Fixing (..), PayReceive (..), Maturity (..), Tenor (..), 
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
import Configuration.CommonTypes.Types as T   

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data Schedule = DrivingSchedule {
                                    schedule :: ScheduleGenLabel
                                }
              | SchEqual2 { equal2 :: SchOption} 
              | SchDeducedFrom {
                                        schDedFrom :: SchOption,
                                        schDedForm :: DateShifterLabel
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
                       tenorUnit :: T.Unit,
                       tenorQuantity :: Int
                   } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Frequency = Annually | SemiAnnually | Quarterly | Monthly
                deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Nominal = Nominal {
                             nomQuantity :: Double,
                             nomCurrency :: Currency
                       }
               deriving (Eq, Show, Data, Typeable)
                 
