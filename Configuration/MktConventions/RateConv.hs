{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.MktConventions.RateConv   
    ( 
     RateConv (..), 
     yld_30360, lin_act360_dis, lin_act360, lin_30360
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Utils.MyJSON
import Configuration.MktConventions.BasisConv as BC

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
                             rc_basis       :: BC.BasisConv,
                             computingMode  :: ComputingMode,
                             rateExpression :: RateExpression,
                             rateQuotation  :: RateQuotation,
                             rateConversion :: Maybe RateConversion
                         } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
yld_30360 = RateConv {
                         rc_basis       = _30E360, 
                         computingMode  = Yield,
                         rateExpression = StandardBasis,
                         rateQuotation  = Annualized,
                         rateConversion = Nothing
                     } 

--------------------------------------------------------------------------
lin_act360 = RateConv {
                          rc_basis       = _ACT360, 
                          computingMode  = Linear,
                          rateExpression = StandardBasis,
                          rateQuotation  = Annualized,
                          rateConversion = Nothing
                      } 

--------------------------------------------------------------------------
lin_30360 = RateConv {
                          rc_basis       = _30E360, 
                          computingMode  = Linear,
                          rateExpression = StandardBasis,
                          rateQuotation  = Annualized,
                          rateConversion = Nothing
                      } 

--------------------------------------------------------------------------
lin_act360_dis = RateConv {
                              rc_basis       = _ACT360, 
                              computingMode  = Linear,
                              rateExpression = DiscountBasis,
                              rateQuotation  = Annualized,
                              rateConversion = Nothing
                          } 

























