{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------

module Configuration.MktConventions.RateConv   
    ( 
     RateConv (..), 
     yld_30360, lin_act360_dis
    ) where

--------------------------------------------------------------------------
import Data.Time.Calendar
import Utils.MyJSON
import Configuration.MktConventions.BasisConv as BC

--------------------------------------------------------------------------
data ComputingMode = Linear	 
                   | DailyComp	
                   | Yield	 
                   | Exponential deriving (Eq, Show, Data, Typeable, Read)

data RateExpression = DiscountBasis	 
                    | StandardBasis deriving (Eq, Show, Data, Typeable, Read)

data RateQuotation = Annualized deriving (Eq, Show, Data, Typeable, Read)

data RateConversion = RateConversion deriving (Eq, Show, Data, Typeable, Read)

data RateConv = RateConv {
                             rc_basis       :: BC.BasisConv,
                             computingMode  :: ComputingMode,
                             rateExpression :: RateExpression,
                             rateQuotation  :: RateQuotation,
                             rateConversion :: Maybe RateConversion
                         } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
yld_30360 = RateConv {
                         rc_basis       = _30E360, 
                         computingMode  = Yield,
                         rateExpression = StandardBasis,
                         rateQuotation  = Annualized,
                         rateConversion = Nothing
                     } 

lin_act360_dis = RateConv {
                              rc_basis       = _ACT360, 
                              computingMode  = Linear,
                              rateExpression = DiscountBasis,
                              rateQuotation  = Annualized,
                              rateConversion = Nothing
                          } 


























