{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}


--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------
module Pricing.Rates.IRSwap
    ( 
    
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Utils.MyJSON
import Utils.MyUtils
import Configuration.Generators.SwapGenerators
import Valuation
import Valuation.MktData

--------------------------------------------------------------------------
------------------------------- Classes ----------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
---------------------------- Instances -----------------------------------   
-------------------------------------------------------------------------- 

instance Valuable Swap where  
    value mktData swap = do
        valLegs <- checkAllOk_ $ fmap (value mktData) (swLegs swap)
        return ValueStorage {
                                vsValue = sum $ fmap vsValue valLegs, 
                                vsSubValues = valLegs
                            }  
    valueGreeks mktData swap = do
        valLegs <- checkAllOk_ $ fmap (valueGreeks mktData) (swLegs swap)
        return ValueStorage {
                                vsValue = sum $ fmap vsValue valLegs, 
                                vsSubValues = valLegs
                            }  
-------------------------------------------------------------------------- 

instance Valuable Leg where  
    value mktData leg = Ok_ ValueStorage {
                                vsValue = 0.0, 
                                vsSubValues = []
                            }  
    valueGreeks mktData swap = Error_ ""
--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------


