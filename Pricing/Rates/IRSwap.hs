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
import Data.List
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
    valueFunction :: v -> MktData -> Leg -> Flow -> Result_ ValueStorage
    valueGreeksFunction :: v -> MktData -> Leg -> Flow -> Result_ ValueStorage

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
    valueA vleg = do
        valFlows <- checkAllOk_ $ fmap (vlValuator vleg) (lFlows $ vlLeg vleg)
        return ValueStorage {
                                vsValue = sum $ fmap vsValue valFlows, 
                                vsSubValues = valFlows
                            }  
    valueAGreeks vleg = do
        valFlows <- checkAllOk_ $ fmap (vlValuatorGreeks vleg) (lFlows $ vlLeg vleg)
        return ValueGreeksStorage {
                                      vgsValue = sum $ fmap vsValue valFlows, 
                                      vgsGreeks = fmap (\ x -> 1) valFlows,
                                      vgsSubValues = valFlows
                                  }   
-------------------------------------------------------------------------- 
instance Valuator (ValuationInfo PayOffStd ModelA) where    
    valueFunction v mktDat = integrateBS mktDat
    valueGreeksFunction v mktDat = integrateBSGreeks mktDat
    
integrateBS :: MktData -> Leg -> Flow -> Result_ ValueStorage
integrateBS md l f = Error_ ""
integrateBSGreeks :: MktData -> Leg -> Flow -> Result_ ValueStorage
integrateBSGreeks md l f = Error_ ""
-------------------------------------------------------------------------- 
-------------------------------------------------------------------------- 

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type ModelLab = String
type PayOffLab = String
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
                         vlModelLab :: ModelLab,
                         vlPayOffLab :: PayOffLab,
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

getValuatorFun :: ValuatorHomog -> MktData -> Leg -> Flow 
               -> Result_ ValueStorage
getValuatorFun (ValuatorHomog (vi)) = valueFunction vi
--------------------------------------------------------------------------    
buildValuatorHomog :: String -> String -> Result_ ValuatorHomog
buildValuatorHomog "PayOffStd" "ModelA" = 
    Ok_ (ValuatorHomog (ValuationInfo PayOffStd ModelA))
-------------------------------------------------------------------------- 
buildValuationLeg :: MktData -> (PayOffLab, ModelLab, Engine, Leg)
                  -> Result_ ValLeg
buildValuationLeg mkt (p, m, e, l) = do

    (ValuatorHomog (valInfo)) <- buildValuatorHomog p m
    return ValLeg {
                      vlLeg = l, 
                      vlEngine = e,
                      vlModelLab = m,
                      vlPayOffLab = p,
                      vlValuator = (valueFunction valInfo) mkt l,
                      vlValuatorGreeks = (valueGreeksFunction valInfo) mkt l
                  }
-------------------------------------------------------------------------- 
buildValuationSwap :: [(PayOffLab, ModelLab, Engine)] -> Swap -> MktData 
                   -> Result_ ValSwap
buildValuationSwap ls@((p, m, e):[(ps, ms, es)]) sw mkt = do
    let legs = swLegs sw
    let numLegs = length legs
    let inputList = if length ls == numLegs
                    then ls
                    else replicate numLegs (p,m,e)
    let inputList2 = zipWith (\ (p,m,e) l -> (p,m,e,l))  inputList legs
    valLegs <- checkAllOk_ $ fmap (buildValuationLeg mkt) inputList2
    return ValSwap {
                       vswSwap = sw, 
                       vswLegs = valLegs
                   }

--------------------------------------------------------------------------
------------------------------ Examples ----------------------------------
-------------------------------------------------------------------------- 
{-leg = swLeg swap
mktData = MktData
valInfo = ValuationInfo payOffA modelA
val = valueFunction valInfo mktData leg
valGreeks = valueGreeksFunction valInfo mktData leg

valLeg = ValLeg leg Analytical val valGreeks

valueA valLeg-}



































