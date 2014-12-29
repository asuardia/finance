{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.Indices.IRIndices   
    ( 
     
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Utils.MyJSON
import Utils.MyUtils
import Configuration.MktConventions.Calendars as Cal
import Configuration.MktConventions.DateAdjustments as DAdj
--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data IRIndex = IRIndex {
                           iriClassification :: Classification,
                           iriFixings        :: Fixings
                       } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Classification = Classification {
                                         clCategory    :: Category,
                                         clDescription :: Maybe String,
                                         clCode        :: Maybe String,
                                         clIndexDef    :: IndexDef
                                     } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data IndexDef = IndexDef {
                             idNature            :: RateNature,
                             idCurrency          :: Currency,
                             idStartDelay        :: DateShifter,
                             idCalcStartSchedule :: CalcStartSchedule,
                             idCalcEndSchedule   :: CalcEndSchedule,
                             idPaymentSchedule   :: PaymentSchedule,
                             idFixingSchedule    :: FixingSchedule,
                             idRateConvention    :: RateConv,
                             idRoundRule         :: RoundingRule
                         } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data RateNature = StandardRate
                | SwapRate{
                              rnGenerator :: String,
                              rnMaturity  :: Maturity  
                          } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Maturity = Maturity {
                             matType   :: TypeMaturity,
                             matNumber :: Int,
                             matUnits  :: MatUnit
                         } deriving (Eq, Show, Data, Typeable)  
--------------------------------------------------------------------------
data TypeMaturity = Tenor deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------
data MatUnit = Year deriving (Eq, Show, Data, Typeable) 
--------------------------------------------------------------------------
data CalcStartSchedule = DrivingSchedule {
                                             dsSchedule :: ScheduleGen
                                         } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data CalcEndSchedule = EndSchEqual2Start deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data PaymentSchedule = PaymentSchEqual2Start
                     | PaymentSchDeducedFromStart {
                                                      pSchDedFromSt :: DateShifter
                                                  } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data FixingSchedule = FixingSchEqual2Start
                    | FixingSchDeducedFromStart {
                                                    fSchDedFromSt :: DateShifter
                                                } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data RoundingRule = None | Nearest | ByExcess | SameAsAbove5
                    deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Fixings = Fixings {
                           fixFormulaType    :: FormulaType,
                           fixArchivingGroup :: ArchivingGroup
                       } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------

data Category = Rate   | EquityPrice | BondPrice | Inflation 
              | FxSpot | PoolFactor  | GenericIndex 
              | Formula {
                            catFormulaType :: FormulaType
                        } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
data FormulaType = Published | Compounded | Average 
                 | Basket | StartEnd deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data ArchivingGroup = EuriborAGr deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------


--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------




