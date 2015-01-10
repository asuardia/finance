{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -XGADTs #-}
--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Generators.SwapGenerators
    (    
    KK (..), JJ, jj, o
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import qualified Data.Map as Map 
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars
import Configuration.Forex.Currencies
import Configuration.MktConventions.ScheduleGen
import Configuration.MktConventions.DateShifters
import Configuration.MktConventions.RateConv
import qualified Configuration.CommonTypes.Types as CT
import qualified Configuration.CommonTypes.TypesProducts as CTP
import Configuration.Indices.IRIndices
import Valuation.Analytical  
import Valuation.MktData  
import Valuation        
--------------------------------------------------------------------------    
data KK k = KK k

class JJ j where
    jj :: j -> Int
    
instance JJ (KK String) where
    jj (KK a) = 0
    
h :: a -> KK a
h a = KK a

o a = jj $ KK a