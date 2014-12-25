{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
module Market.FinantialConventions   
    ( 
     FracConvention (..), CoumpondingConvention (..), BuySell (..), 
     PayerReceiver (..),  Convention (..),            RateNature (..), 
     StartDelay (..),     ScheduleGenerator (..),     Period (..), 
     TimeStep (..),       Index (..),                 PayFreq (..), 
     PayMode (..),        FixMode (..),               Calendar (..), 
     Schedules (..),      BusinessDayConvention (..), Currency (..)   
    ) where
    
import Data.Time.Calendar    
import Utils.MyJSON

--------------------------------------------------------------------------
-- Some finantial conventions

data Currency = EUR 
              | USD 
              | GBP deriving (Eq, Show, Data, Typeable, Read)
--------------------------------------------------------------------------

data FracConvention = ACTACT 
                    | ACT360 
                    | ACT365 
                    | THIRTY360 deriving (Eq, Show, Data, Typeable, Read)
                    
--------------------------------------------------------------------------
data CoumpondingConvention = LIN 
                           | YIELD deriving (Eq, Show, Data, Typeable, Read)
                           
--------------------------------------------------------------------------
data BuySell = BUY 
             | SELL deriving (Eq, Show, Data, Typeable, Read)
             
--------------------------------------------------------------------------
data PayerReceiver = PAYER 
                   | RECEIVER deriving (Eq, Show, Data, Typeable, Read)
                   
--------------------------------------------------------------------------
type Convention = (CoumpondingConvention, FracConvention)

--------------------------------------------------------------------------
data RateNature = Standard 
                | SwapRate deriving (Eq, Show, Data, Typeable, Read)
                
--------------------------------------------------------------------------
data StartDelay = OpenDays{nDays :: Int} 
                  deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data ScheduleGenerator = ModFol{
                                   sgUnits  :: Int, 
                                   sgPeriod :: Period
                               } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Period = Day 
            | Week 
            | Month 
            | Year deriving (Eq, Show, Data, Typeable, Read)
            
--------------------------------------------------------------------------
data TimeStep = TimeStep {
                             tUnits  :: Int, 
                             tPeriod :: Period
                         } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Index = Index {
                       iName              :: String, 
                       iDescription       :: String, 
                       iRateNaure         :: RateNature, 
                       iTenor             :: TimeStep,
                       iStartDelay        :: StartDelay, 
                       iConvention        :: Convention, 
                       iScheduleGenerator :: ScheduleGenerator
                   } deriving (Eq, Show, Data, Typeable, Read)
                   
--------------------------------------------------------------------------
data PayFreq = Annually | SemiAnnually | Quarterly | Monthly 
               deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------
data PayMode = PayUpFront | PayInArrears deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------  
data FixMode = FixUpFront | FixInArrears deriving (Eq, Show, Data, Typeable)   
--------------------------------------------------------------------------
data Calendar = Target deriving (Eq, Show, Data, Typeable)          
--------------------------------------------------------------------------
data Schedules = Schedules {
                                sTimeStep       :: TimeStep,
                                sBusinessDayCon :: BusinessDayConvention
                           }deriving (Eq, Show, Data, Typeable)         
--------------------------------------------------------------------------
data BusinessDayConvention = Following 
                           | ModifiedFollowing 
                           | Preceding
                           | ModifiedPreceding
                           | Unadjusted
                           deriving (Eq, Show, Data, Typeable)                    
                   

                   
                   
                   
                   
