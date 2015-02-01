{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.MktConventions.BasisConv   
    ( 
     BasisConv (..), Numerator (..), Denominator (..),
     TimeUnits (..), TypeInclExcl (..), EOMAdjust (..), Basis (..), 
     FullPeriodInterp (..), Breakdown (..), PeriodDet (..), Periodicity (..),
     BasisConvLabel,
     yearFrac, yearFraction, numeratorYearFrac, denominatorYearFrac,
     numeratorYearFraction, denominatorYearFraction,
     actact, actact_isda, act360, act365, _30360, _30e360_german, _30e360,
     bcACTACT, bcACTACT_ISDA, bcACT360, bcACT365, bc30360, bc30E360_GERMAN, bc30E360
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Utils.MyJSON
import Utils.MyUtils

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------

type BasisConvLabel = String
type NumMonth = Int
type NumDay   = Int

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------

data Numerator = N_ACT 
                 | N_30 | N_30_E_AFB | N_30_E_ISDA | N_30_E_GERMAN | N_30_EPLUS	
                 | N_NL deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Denominator = D_ACT 
                 | D_30 | D_30_E | D_30_EPLUS	 
                 | D_360 
                 | D_364	 
                 | D_365 | D_365_25
                 | D_366 | D_366_1 | D_366_2
                 | D_NL deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data TimeUnits = Days	 
               | NoWeekendDays	 
               | BusinessDays	 
               | Months	 
               | Quarters	 
               | Semesters deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data BasisConv = BasisConv {
                               bc_basis   :: Basis,
                               fullPeriod :: Maybe FullPeriodInterp,
                               breakdown  :: Maybe Breakdown
                           } deriving (Eq, Show, Data, Typeable, Read)


--------------------------------------------------------------------------
data TypeInclExcl = ExclusiveT2_T1
                  | InclusiveT2_T1_plus_1
                  | T2_T1
                  | T2_T1_1 deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data EOMAdjust = Yes
               | No
               | Yes_28 deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Basis = Basis {
                       numerator      :: Numerator,
                       denominator    :: Denominator,
                       unit           :: TimeUnits,
                       inclexcl       :: TypeInclExcl,
                       capped         :: Bool,
                       fstPeriodBasis :: Maybe Basis,
                       lstPeriodBasis :: Maybe Basis
                   } deriving (Eq, Show, Data, Typeable, Read)


--------------------------------------------------------------------------
data FullPeriodInterp = AppStdFormula
                      | R_plus_1 deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Breakdown = Breakdown {
                               periodDet   :: PeriodDet,
                               periodicity :: Periodicity,
                               eomAdjust   :: EOMAdjust
                           } deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data PeriodDet = Forward
               | Backward
               | ISDA
               | Forward_ISMA
               | Backward_ISMA deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
data Periodicity = Year
                 | Semester
                 | Quarter
                 | Month
                 | CouponPer deriving (Eq, Show, Data, Typeable, Read)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
idBasisConv :: BasisConvLabel -> Result_ BasisConv
idBasisConv "ACTACT"   = Ok_ actact
idBasisConv "ACTACT_ISDA" = Ok_ actact_isda
idBasisConv "ACT360"   = Ok_ act360
idBasisConv "ACT365" = Ok_ act365
idBasisConv "30360"   = Ok_ _30360
idBasisConv "30E360_GERMAN" = Ok_ _30e360_german
idBasisConv "30E360"   = Ok_ _30e360
idBasisConv bc = Error_ (" idBasisConv: " ++ bc ++ "Not identified basis convention. ")
--------------------------------------------------------------------------
yearFraction :: BasisConvLabel -> Day -> Day -> Result_ Double
yearFraction basConvLab dt1 dt2   = do 
    basConv <- idBasisConv basConvLab
    yearFrac basConv dt1 dt2
--------------------------------------------------------------------------
numeratorYearFraction :: BasisConvLabel -> Day -> Day -> Result_ Double
numeratorYearFraction basConvLab dt1 dt2   = do 
    basConv <- idBasisConv basConvLab
    numeratorYearFrac basConv dt1 dt2
--------------------------------------------------------------------------
denominatorYearFraction :: BasisConvLabel -> Day -> Day -> Result_ Double
denominatorYearFraction basConvLab dt1 dt2   = do 
    basConv <- idBasisConv basConvLab
    denominatorYearFrac basConv dt1 dt2

--------------------------------------------------------------------------
yearFrac :: BasisConv -> Day -> Day -> Result_ Double
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = D_ACT,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           },          
                          breakdown = Just (Breakdown {
                                                         periodDet   = Forward,
                                                         periodicity = period,
                                                         eomAdjust   = eom
                                                     })
                          
                      } 
         d1 d2 = do 
            let numDays = diffDays d2 d1
            denDays    <- cntDaysYear True period eom d1
            return ((fromIntegral numDays)/(fromIntegral denDays))

          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = D_ACT,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           },          
                          breakdown = Just (Breakdown {
                                                         periodDet   = Backward,
                                                         periodicity = period,
                                                         eomAdjust   = eom
                                                     })
                          
                      } 
         d1 d2 = do 
            let numDays = diffDays d2 d1
            denDays    <- cntDaysYear False period eom d1
            return $ (fromIntegral numDays)/(fromIntegral denDays)
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = D_ACT,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           },          
                          breakdown = Just (Breakdown {
                                                         periodDet   = ISDA,
                                                         periodicity = period,
                                                         eomAdjust   = eom
                                                     })
                          
                      } 
         d1 d2 = do
            let (y1, _, _) = toGregorian d1
            let (y2, _, _) = toGregorian d2
            actD1 <- civilPeriod period y1 False
            actD2 <- civilPeriod period y2 True
            return (if y1 /= y2 
                    then let fstDayYear = fromGregorian y2 1 1
                             lstDayYear = fromGregorian y1 12 31
                             actN1 = fromIntegral $ diffDays fstDayYear d1 
                             actN2 = fromIntegral $ diffDays d2 lstDayYear 
                         in actN1 / actD1 + actN2 / actD2                         
                    else let actN  = fromIntegral $ diffDays d2 d1
                             ene1  = fromGregorian y1 1 1
                             ene1' = fromGregorian y2 1 1
                             actD  = fromIntegral $ diffDays ene1' ene1 
                         in actN / actD)
            where civilPeriod :: Periodicity -> Integer -> Bool -> Result_ Double
                  civilPeriod Year     y isIni = Ok_ (if isIni 
                                                     then 1 * (fromIntegral $ diffDays 
                                                               (fromGregorian y 1 1)
                                                               (fromGregorian (y-1) 12 31))
                                                     else 1 * (fromIntegral $ diffDays 
                                                               (fromGregorian y 1 1)
                                                               (fromGregorian (y-1) 12 31)))
                  civilPeriod Semester y isIni = Ok_ (if isIni 
                                                     then 2 * (fromIntegral $ diffDays 
                                                               (fromGregorian y 6 31) 
                                                               (fromGregorian (y-1) 12 31))
                                                     else 2 * (fromIntegral $ diffDays 
                                                               (fromGregorian (y+1) 1 1) 
                                                               (fromGregorian y 7 1)))
                  civilPeriod Quarter  y isIni = Ok_ (if isIni 
                                                     then 4 * (fromIntegral $ diffDays 
                                                               (fromGregorian y 3 31) 
                                                               (fromGregorian (y-1) 12 31))
                                                     else 4 * (fromIntegral $ diffDays 
                                                               (fromGregorian (y+1) 1 1) 
                                                               (fromGregorian y 10 1)))
                  civilPeriod Month    y isIni = Ok_ (if isIni 
                                                     then 12 * (fromIntegral $ diffDays 
                                                               (fromGregorian y 1 31) 
                                                               (fromGregorian (y-1) 12 31))
                                                     else 12 * (fromIntegral $ diffDays 
                                                               (fromGregorian (y+1) 1 1) 
                                                               (fromGregorian y 12 1)))
                  civilPeriod _        y isIni = Error_ " civilPeriod': Periodicity not implemented " 
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = D_ACT,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           },          
                          breakdown = Just (Breakdown {
                                                         periodDet   = Forward_ISMA,
                                                         periodicity = period,
                                                         eomAdjust   = eom
                                                     })
                          
                      } 
         d1 d2 = do 
            periodsYear <- buildFracYear period eom True d1
            yF          <- yearFrac' period
            let ini = init periodsYear
            let end = tail periodsYear
            let iniLess = takeWhile ((<=) d2) ini
            let fracsYear = calFracs (zip iniLess end) yF 
            return fracsYear
            where calFracs :: [(Day, Day)] -> Double -> Double
                  calFracs (l:[]) yF = yF * ((fromIntegral $ diffDays d2 (fst l))
                                  / (fromIntegral $ diffDays (snd l) (fst l)))
                  calFracs (l:ls) yF = yF + calFracs ls yF
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = D_ACT,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           },          
                          breakdown = Just (Breakdown {
                                                         periodDet   = Backward_ISMA,
                                                         periodicity = period,
                                                         eomAdjust   = eom
                                                     })
                          
                      } 
         d1 d2 = do 
            periodsYear <- buildFracYear period eom False d2
            yF          <- yearFrac' period
            let end = init periodsYear
            let ini = tail periodsYear
            let endGreat = takeWhile ((<=) d1) end
            let fracsYear = calFracs (zip ini endGreat) yF 
            return fracsYear
            where calFracs :: [(Day, Day)] -> Double -> Double
                  calFracs (l:[]) yF = yF * ((fromIntegral $ diffDays (snd l) d1 )
                                  / (fromIntegral $ diffDays (snd l) (fst l)))
                  calFracs (l:ls) yF = yF + calFracs ls yF
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = N_ACT,
                                               denominator = den,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           }          
                      } 
         d1 d2 = do 
            denDays <- countBase3 den d1 d2
            return ((fromIntegral numDays) / denDays)
    where numDays = (toModifiedJulianDay d2 - toModifiedJulianDay d1)
          
          ---------------------------------
yearFrac bc@BasisConv { 
                          bc_basis = Basis {
                                               numerator   = num,
                                               denominator = den,
                                               unit        = Days,
                                               inclexcl    = ExclusiveT2_T1                                               
                                           }          
                      } 
         d1 d2 = do 
            numDays <- countNum3  num d1 d2
            denDays <- countBase3 den d1 d2
            return (numDays / denDays)
          
          ---------------------------------
yearFrac bc d1 d2 = Error_ $ " yearFrac: Not implemented: "
                             ++ show bc ++ ".  "
--------------------------------------------------------------------------
numeratorYearFrac :: BasisConv -> Day -> Day -> Result_ Double
          ---------------------------------
numeratorYearFrac bc@BasisConv { 
                                  bc_basis = Basis {
                                                       numerator   = N_ACT,
                                                       unit        = Days,
                                                       inclexcl    = ExclusiveT2_T1                                               
                                                   }          
                               } 
         d1 d2 = do 
            return (fromIntegral numDays)
    where numDays = (toModifiedJulianDay d2 - toModifiedJulianDay d1)
          ---------------------------------
numeratorYearFrac bc d1 d2 = Error_ $ " numeratorYearFrac: Not implemented: "
                                      ++ show bc ++ ".  "
--------------------------------------------------------------------------
denominatorYearFrac :: BasisConv -> Day -> Day -> Result_ Double
          ---------------------------------
denominatorYearFrac bc@BasisConv {bc_basis = Basis {denominator = D_360}} 
         d1 d2 = Ok_ 360.0 
          ---------------------------------
denominatorYearFrac bc@BasisConv {bc_basis = Basis {denominator = D_364}} 
         d1 d2 = Ok_ 364.0 
          ---------------------------------
denominatorYearFrac bc@BasisConv {bc_basis = Basis {denominator = D_365}} 
         d1 d2 = Ok_ 365.0 
          ---------------------------------
denominatorYearFrac bc@BasisConv {bc_basis = Basis {denominator = D_366}} 
         d1 d2 = Ok_ 366.0 
          ---------------------------------
denominatorYearFrac bc d1 d2 = Error_ $ " denominatorYearFrac: Not implemented: "
                                        ++ show bc ++ ".  "
--------------------------------------------------------------------------
addMonthEOM :: EOMAdjust -> Day -> Integer -> Result_ Day
addMonthEOM Yes dt nM = Ok_ (pass2EOM $ addGregorianMonthsClip nM dt)
addMonthEOM No dt nM  = Ok_ (addGregorianMonthsRollOver nM dt)
addMonthEOM _ _ _  = Error_ " addMonthEOM: EOM not implemented "
--------------------------------------------------------------------------
cntDaysYear :: Bool -> Periodicity -> EOMAdjust -> Day -> Result_ Integer
cntDaysYear isFor per eom dt     = do 
    (nMonths, nFractions) <- numMonths per
    shDt <- addMonthEOM eom dt nMonths
    return ((diffDays shDt dt) * nFractions)
    where 
        dir = if isFor then 1 else (-1)
        numMonths Year = do 
            return ((dir * 12), 1)
        numMonths Semester = do 
            return ((dir * 6), 2)
        numMonths Quarter = do 
            return ((dir * 3), 4)
        numMonths Month = do 
            return ((dir * 1), 12)
        numMonths _ = do 
            Error_ " cntDaysYear: Periodicity not implemented "
            
            
--------------------------------------------------------------------------
yearFrac' :: Periodicity -> Result_ Double
yearFrac' Year     = Ok_ 1
yearFrac' Semester = Ok_ 0.5
yearFrac' Quarter  = Ok_ 0.25
yearFrac' Month    = Ok_ (1/6)
yearFrac' _        = Error_ " yearFrac': Periodicity not implemented " 
--------------------------------------------------------------------------
buildFracYear :: Periodicity -> EOMAdjust ->  Bool -> Day -> Result_ [Day]
buildFracYear Year eom isFor dt  = do
    let dir = if isFor then [0, 1] else [0, -1] 
    concatResult $ fmap (addMonthEOM eom dt) dir
buildFracYear Semester eom isFor dt  = do
    let dir = if isFor then [0 .. 2] else [0, -1 .. -2] 
    concatResult $ fmap (addMonthEOM eom dt) dir
buildFracYear Quarter eom isFor dt  = do
    let dir = if isFor then [0 ..4] else [0, -1 .. -4] 
    concatResult $ fmap (addMonthEOM eom dt) dir
buildFracYear Month eom isFor dt  = do
    let dir = if isFor then [0 ..12] else [0, -1 .. -12] 
    concatResult $ fmap (addMonthEOM eom dt) dir
buildFracYear _ eom isFor dt  = do
    Error_ " buildFracYear: Periodicity not implemented "
--------------------------------------------------------------------------
countBase3 :: Denominator -> Day -> Day -> Result_ Double
countBase3 D_360    d1 d2 = Ok_ 360.0
countBase3 D_364    d1 d2 = Ok_ 364.0
countBase3 D_365    d1 d2 = Ok_ 365.0
countBase3 D_365_25 d1 d2 = Ok_ 365.25
countBase3 D_366    d1 d2 = Ok_ 366.0
countBase3 D_366_1  d1 d2 = if (isLeapYear $ fst3 $ toGregorian d1)
                            then Ok_ 366
                            else Ok_ 365
countBase3 D_366_2  d1 d2 = if (isLeapYear $ fst3 $ toGregorian d2)
                            then Ok_ 366
                            else Ok_ 365
countBase3 den      d1 d2 = Error_ $ " countBase3: No such Base Convention: "
                                     ++ show den ++ ".  "
--------------------------------------------------------------------------

countNum3 :: Numerator -> Day -> Day -> Result_ Double
countNum3 N_30          dt1 dt2 = Ok_ $ (monthAndYearFraction dt1 dt2)
   + let d1G = toGregorian dt1 
         d2G = toGregorian dt2
         d1 = fromIntegral $ trd3 d1G 
         d2 = fromIntegral $ trd3 d2G 
         m1 = fromIntegral $ snd3 d1G 
         m2 = fromIntegral $ snd3 d2G
         d1Sh = if d1 == 31 then 30 else d1
         d2Sh = if (d2 == 31) && ((d1 == 30) || (d1 == 31)) then 30 else d2
     in d2Sh - d1Sh
          ---------------------------------
countNum3 N_30_E_AFB    dt1 dt2 = Ok_ $ (monthAndYearFraction dt1 dt2)
   + let d1G = toGregorian dt1 
         d2G = toGregorian dt2
         d1 = fromIntegral $ trd3 d1G 
         d2 = fromIntegral $ trd3 d2G 
         m1 = fromIntegral $ snd3 d1G 
         m2 = fromIntegral $ snd3 d2G
         d1Sh = if d1 == 31 then 30 else d1
         d2Sh = if d2 == 31 then 30 else d2 
     in d2Sh - d1Sh
          ---------------------------------
countNum3 N_30_E_ISDA   dt1 dt2 = Ok_ $ (monthAndYearFraction dt1 dt2)
   + let d1G = toGregorian dt1 
         d2G = toGregorian dt2
         d1 = fromIntegral $ trd3 d1G 
         d2 = fromIntegral $ trd3 d2G 
         m1 = fromIntegral $ snd3 d1G 
         m2 = fromIntegral $ snd3 d2G
         d1Sh = if d1 == 31 then 30 else d1
         d2Sh = if d2 == 31 then 30 else adjFebr dt2
     in d2Sh - d1Sh
     where adjFebr dt = let (y, m, d) = toGregorian dt
                            isLeap    = isLeapYear y
                            dSh = if (((m == 2) && isLeap && (d == 29) ) 
                                     || ((m == 2) && not isLeap && (d == 28) ))
                                  then 30 
                                  else d
                        in fromIntegral dSh
          ---------------------------------
countNum3 N_30_E_GERMAN dt1 dt2 = Ok_ $ (monthAndYearFraction dt1 dt2)
   + let d1G = toGregorian dt1 
         d2G = toGregorian dt2
         d1 = fromIntegral $ trd3 d1G 
         d2 = fromIntegral $ trd3 d2G 
         m1 = fromIntegral $ snd3 d1G 
         m2 = fromIntegral $ snd3 d2G
         d1Sh = if d1 == 31 then 30 else adjFebr dt1
         d2Sh = if d2 == 31 then 30 else adjFebr dt2
     in d2Sh - d1Sh
     where adjFebr dt = let (y, m, d) = toGregorian dt
                            isLeap    = isLeapYear y
                            dSh = if (((m == 2) && isLeap && (d == 29) ) 
                                     || ((m == 2) && not isLeap && (d == 28) ))
                                  then 30 
                                  else d
                        in fromIntegral dSh
          ---------------------------------
countNum3 N_30_EPLUS    dt1 dt2 = Ok_ $ (monthAndYearFraction dt1 dt2)
   + let d1G = toGregorian dt1 
         d2G = toGregorian dt2
         d1 = fromIntegral $ trd3 d1G 
         d2 = fromIntegral $ trd3 d2G 
         m1 = fromIntegral $ snd3 d1G 
         m2 = fromIntegral $ snd3 d2G
         d1Sh = if d1 == 31 then 30 else d1
         d2Sh = if d2 == 31 then 32 else d2
     in d2Sh - d1Sh
countNum3 num           dt1 dt2 = Error_ $ " countNum3: No such Base Convention: "
                                     ++ show num ++ ".  "
--------------------------------------------------------------------------
monthAndYearFraction :: Day -> Day -> Double
monthAndYearFraction dt1 dt2 = let m2 = fromIntegral $ snd3 $ toGregorian dt2
                                   m1 = fromIntegral $ snd3 $ toGregorian dt1
                                   y2 = fromIntegral $ fst3 $ toGregorian dt2
                                   y1 = fromIntegral $ fst3 $ toGregorian dt1
                               in (y2 - y1) * 360 + (m2 - m1) * 30
                               
                               
extractDayMonth :: Day -> Day -> (Double, Double, Double, Double)
extractDayMonth dt1 dt2 = (d1, m1, d2, m2)
    where d1G = toGregorian dt1 
          d2G = toGregorian dt2
          d1 = fromIntegral $ trd3 d1G 
          d2 = fromIntegral $ trd3 d2G 
          m1 = fromIntegral $ snd3 d1G 
          m2 = fromIntegral $ snd3 d2G                                
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
bcACTACT = "ACTACT"
actact = BasisConv {
                       bc_basis = Basis {
                                             numerator      = N_ACT,
                                             denominator    = D_ACT,
                                             unit           = Days,
                                             inclexcl       = ExclusiveT2_T1,
                                             capped         = False,
                                             fstPeriodBasis = Nothing,
                                             lstPeriodBasis = Nothing
                                        }, 
                       fullPeriod = Nothing,
                       breakdown  = Just Breakdown {
                                                       periodDet   = Backward,
                                                       periodicity = Year,
                                                       eomAdjust   = No
                                                   }
                  } 

--------------------------------------------------------------------------

bcACTACT_ISDA = "ACTACT_ISDA"
actact_isda = BasisConv {
                             bc_basis = Basis {
                                                   numerator      = N_ACT,
                                                   denominator    = D_ACT,
                                                   unit           = Days,
                                                   inclexcl       = ExclusiveT2_T1,
                                                   capped         = False,
                                                   fstPeriodBasis = Nothing,
                                                   lstPeriodBasis = Nothing
                                              }, 
                             fullPeriod = Nothing,
                             breakdown  = Just Breakdown {
                                                             periodDet   = ISDA,
                                                             periodicity = Year,
                                                             eomAdjust   = No
                                                         }
                        } 


--------------------------------------------------------------------------

bcACT360 = "ACT360"
act360 = BasisConv {
                       bc_basis = Basis {
                                             numerator      = N_ACT,
                                             denominator    = D_360,
                                             unit           = Days,
                                             inclexcl       = ExclusiveT2_T1,
                                             capped         = False,
                                             fstPeriodBasis = Nothing,
                                             lstPeriodBasis = Nothing
                                        }, 
                       fullPeriod = Nothing,
                       breakdown  = Nothing
                  } 

--------------------------------------------------------------------------

bcACT365 = "ACT365"
act365 = BasisConv {
                       bc_basis = Basis {
                                             numerator      = N_ACT,
                                             denominator    = D_365,
                                             unit           = Days,
                                             inclexcl       = ExclusiveT2_T1,
                                             capped         = False,
                                             fstPeriodBasis = Nothing,
                                             lstPeriodBasis = Nothing
                                        }, 
                       fullPeriod = Nothing,
                       breakdown  = Nothing
                  } 

--------------------------------------------------------------------------

bc30360 = "30360"
_30360 = BasisConv {
                       bc_basis = Basis {
                                             numerator      = N_30,
                                             denominator    = D_360,
                                             unit           = Days,
                                             inclexcl       = ExclusiveT2_T1,
                                             capped         = False,
                                             fstPeriodBasis = Nothing,
                                             lstPeriodBasis = Nothing
                                        }, 
                       fullPeriod = Nothing,
                       breakdown  = Nothing
                  } 


--------------------------------------------------------------------------
bc30E360 = "30E360"
_30e360 = BasisConv {
                       bc_basis = Basis {
                                             numerator      = N_30_E_AFB,
                                             denominator    = D_360,
                                             unit           = Days,
                                             inclexcl       = ExclusiveT2_T1,
                                             capped         = False,
                                             fstPeriodBasis = Nothing,
                                             lstPeriodBasis = Nothing
                                        }, 
                       fullPeriod = Nothing,
                       breakdown  = Nothing
                  } 


--------------------------------------------------------------------------
bc30E360_GERMAN = "30E360_GERMAN"
_30e360_german = BasisConv {
                               bc_basis = Basis {
                                                     numerator      = N_30_E_GERMAN,
                                                     denominator    = D_360,
                                                     unit           = Days,
                                                     inclexcl       = ExclusiveT2_T1,
                                                     capped         = False,
                                                     fstPeriodBasis = Nothing,
                                                     lstPeriodBasis = Nothing
                                                }, 
                               fullPeriod = Nothing,
                               breakdown  = Nothing
                          } 


--------------------------------------------------------------------------
-------------------------------- TESTS -----------------------------------
--------------------------------------------------------------------------

ex1 = yearFraction bcACTACT (fromGregorian 2014 3 1) (fromGregorian 2014 1 1)
ex2 = yearFraction bc30E360 (fromGregorian 2014 3 1) (fromGregorian 2014 1 1)
ex3 = yearFraction bc30360 (fromGregorian 2014 3 1) (fromGregorian 2014 1 1)
ex4 = yearFraction bcACT360 (fromGregorian 2014 3 1) (fromGregorian 2014 1 1)
ex5 = yearFraction bcACT365 (fromGregorian 2014 3 1) (fromGregorian 2014 1 1)





































