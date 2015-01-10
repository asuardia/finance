{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
{-# OPTIONS_GHC -XGADTs #-}
{-# OPTIONS_GHC -XDatatypeContexts #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}


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
import Valuation.Analytical
import Valuation.MktData

--------------------------------------------------------------------------
------------------------------- Classes ----------------------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

class Valuator v where
    valueFunction :: v -> Leg -> Flow -> Result_ ValueStorage
    valueGreeksFunction :: v -> Leg -> Flow -> Result_ ValueStorage

--------------------------------------------------------------------------
---------------------------- Instances -----------------------------------   
-------------------------------------------------------------------------- 

instance Valuable ValSwap where  
    value swap = do
        valLegs <- checkAllOk_ $ fmap value (vswLegs swap)
        return ValueStorage {
                                vsValue = sum $ fmap vsValue valLegs, 
                                vsSubValues = valLegs
                            }  
    valueGreeks swap = do
        valLegs <- checkAllOk_ $ fmap valueGreeks (vswLegs swap)
        return ValueGreeksStorage {
                                      vgsValue = sum $ fmap vsValue valLegs,
                                      vgsGreeks = fmap (\ x -> 1) valLegs,
                                      vgsSubValues = valLegs
                                  }  
-------------------------------------------------------------------------- 

instance Valuable ValLeg where  
    value l@ValLeg{vlEngine = Analytical} = valueA l
    valueGreeks l@ValLeg{vlEngine = Analytical} = valueAGreeks l
-------------------------------------------------------------------------- 

instance AnalyticValuable ValLeg where  
    valueA leg = do
        valFlows <- checkAllOk_ $ fmap (vlValuator leg) (lFlows $ vlLeg leg)
        return ValueStorage {
                                vsValue = sum $ fmap vsValue valFlows, 
                                vsSubValues = valFlows
                            }  
    valueAGreeks leg = do
        valFlows <- checkAllOk_ $ fmap (vlValuatorGreeks leg) (lFlows $ vlLeg leg)
        return ValueGreeksStorage {
                                      vgsValue = sum $ fmap vsValue valFlows, 
                                      vgsGreeks = fmap (\ x -> 1) valFlows,
                                      vgsSubValues = valFlows
                                  }   
-------------------------------------------------------------------------- 
instance Valuator (ValuationInfo PayOffStd ModelA) where    
    valueFunction v = integrateBS
    valueGreeksFunction v = integrateBSGreeks
    
integrateBS :: Leg -> Flow -> Result_ ValueStorage
integrateBS l f = Error_ ""
integrateBSGreeks :: Leg -> Flow -> Result_ ValueStorage
integrateBSGreeks l f = Error_ ""
-------------------------------------------------------------------------- 
-------------------------------------------------------------------------- 

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data ValuatorHomog = forall v . Valuator v => ValuatorHomog v                                       


--------------------------------------------------------------------------
data ValSwap = ValSwap {
                           vswSwap :: Swap, 
                           vswLegs :: [ValLeg]
                       }
--------------------------------------------------------------------------
data ValLeg = ValLeg {
                         vlLeg :: Leg, 
                         vlEngine :: Engine,
                         vlValuator :: Flow -> Result_ ValueStorage,
                         vlValuatorGreeks :: Flow -> Result_ ValueStorage
                     }
--------------------------------------------------------------------------
data PayOffStd = PayOffStd
--------------------------------------------------------------------------
data ModelA = ModelA
--------------------------------------------------------------------------
instance PayOff PayOffStd where
    evalPO pf a = 0.0


--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------    

getValuatorFun :: ValuatorHomog -> Leg -> Flow -> Result_ ValueStorage
getValuatorFun (ValuatorHomog (vi)) = valueFunction vi
--------------------------------------------------------------------------    
buildValuator :: String -> String -> Result_ ValuatorHomog
buildValuator "PayOffStd" "ModelA" = 
    Ok_ (ValuatorHomog (ValuationInfo PayOffStd ModelA))