{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.MktConventions.RateConv   
    ( 
     RateConv (..), RateConvLabel,
     idRateConv,
     rcYLD_30360, rcLIN_ACT360_DIS, rcLIN_ACT360, rcLIN_30360,
     yld_30360, lin_act360_dis, lin_act360, lin_30360
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.BasisConv as BC

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------

type RateConvLabel = String
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data ComputingMode = Linear	 
                   | DailyComp	
                   | Yield	 
                   | Exponential deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data RateExpression = DiscountBasis	 
                    | StandardBasis deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data RateQuotation = Annualized | OtherRQ deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data RateConversion = RateConversion | OtherRC deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data RateConv = RateConv {
                             rc_basis       :: BC.BasisConvLabel,
                             computingMode  :: ComputingMode,
                             rateExpression :: RateExpression,
                             rateQuotation  :: RateQuotation,
                             rateConversion :: Maybe RateConversion
                         } deriving (Eq, Show, Data, Typeable, Read)
                         
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
idRateConv :: RateConvLabel -> Result_ RateConv
idRateConv "YLD_30360"   = Ok_ yld_30360
idRateConv "LIN_ACT360" = Ok_ lin_act360
idRateConv "LIN_30360"   = Ok_ lin_30360
idRateConv "LIN_ACT360_DIS" = Ok_ lin_act360_dis
idRateConv rc = Error_ (" idRateConv: " ++ rc ++ "Not identified rate convention. ")

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
rcYLD_30360 = "YLD_30360"
yld_30360 = RateConv {
                         rc_basis       = bc30E360, 
                         computingMode  = Yield,
                         rateExpression = StandardBasis,
                         rateQuotation  = Annualized,
                         rateConversion = Nothing
                     } 

--------------------------------------------------------------------------
rcLIN_ACT360 = "LIN_ACT360"
lin_act360 = RateConv {
                          rc_basis       = bcACT360, 
                          computingMode  = Linear,
                          rateExpression = StandardBasis,
                          rateQuotation  = Annualized,
                          rateConversion = Nothing
                      } 

--------------------------------------------------------------------------
rcLIN_30360 = "LIN_30360"
lin_30360 = RateConv {
                          rc_basis       = bc30E360, 
                          computingMode  = Linear,
                          rateExpression = StandardBasis,
                          rateQuotation  = Annualized,
                          rateConversion = Nothing
                      } 

--------------------------------------------------------------------------
rcLIN_ACT360_DIS = "LIN_ACT360_DIS"
lin_act360_dis = RateConv {
                              rc_basis       = bcACT360, 
                              computingMode  = Linear,
                              rateExpression = DiscountBasis,
                              rateQuotation  = Annualized,
                              rateConversion = Nothing
                          } 

























