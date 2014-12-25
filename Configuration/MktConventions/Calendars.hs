{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------
--------------------------------- Module ---------------------------------
--------------------------------------------------------------------------

module Configuration.MktConventions.Calendars   
    ( 
     Calendar (..), WeekDay (..), Holidays (..),
     isInCalendar, isWeekEnd,    
     target, new_york
    ) where

--------------------------------------------------------------------------
------------------------------- Imports ----------------------------------
--------------------------------------------------------------------------
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Utils.MyJSON

--------------------------------------------------------------------------
-------------------------------- Alias -----------------------------------
--------------------------------------------------------------------------
type NumMonth = Int
type NumDay   = Int

--------------------------------------------------------------------------
------------------------------- Data -------------------------------------
--------------------------------------------------------------------------
data WeekDay = Monday	 
             | Tuesday	 
             | Wednesday	 
             | Thursday	 
             | Friday	 
             | Saturday	 
             | Sunday deriving (Eq, Show, Data, Typeable, Enum, Read)
--------------------------------------------------------------------------
data Holidays = Holidays {
                             perpetual :: [(NumMonth, NumDay)],
                             single :: [Day]
                         } deriving (Eq, Show, Data, Typeable)
--------------------------------------------------------------------------
data Calendar = Calendar {
                             description :: Maybe String,
                             swiftCode   :: Maybe String,
                             weekEnd     :: [WeekDay],
                             holidays    :: Maybe Holidays
                         } 
              | CalendarUnion {
                                  calUnion :: [Calendar]
                              } deriving (Eq, Show, Data, Typeable)

--------------------------------------------------------------------------
------------------------------ Functions ---------------------------------
--------------------------------------------------------------------------
isInCalendar :: Calendar -> Day -> Bool
isInCalendar Calendar {
                          weekEnd = wds,
                          holidays = Nothing  
                      }
             dt = isWnd wds dt
          ---------------------------------
isInCalendar Calendar {
                          weekEnd = wds,
                          holidays = Just hols  
                      }
             dt = isWnd wds dt || isHol hols dt
          ---------------------------------
isInCalendar CalendarUnion {
                               calUnion = cals
                           }
             dt = any id $ fmap (flip isInCalendar dt) cals
    
--------------------------------------------------------------------------
isHol :: Holidays -> Day -> Bool
isHol (Holidays {perpetual = ps, single = ss}) dt = 
    let (y, m, d) = toGregorian dt in (any id $ fmap ((==) (m,d)) ps)
                                   || (any id $ fmap ((==) dt) ss)
        
--------------------------------------------------------------------------
isWnd :: [WeekDay] -> Day -> Bool
isWnd wds dt = let (y, w, d) = toWeekDate dt 
               in any id $ fmap ((==) (num2wd d)) wds

--------------------------------------------------------------------------
num2wd :: Int -> WeekDay
num2wd 1 = Monday 
num2wd 2 = Tuesday 
num2wd 3 = Wednesday 
num2wd 4 = Thursday 
num2wd 5 = Friday 
num2wd 6 = Saturday 
num2wd 7 = Sunday 
--------------------------------------------------------------------------
isWeekEnd :: Day -> Bool
isWeekEnd dt = let (y, w, d) = toWeekDate dt 
               in any id $ fmap ((==) d) [6,7]
--------------------------------------------------------------------------
---------------------- Standard expressions ------------------------------
--------------------------------------------------------------------------
target = Calendar {
                      description = Just "TARGET", 
                      swiftCode   = Nothing,
                      weekEnd     = [Saturday, Sunday],
                      holidays    = Just (Holidays {
                                                     perpetual = [(12, 24)],
                                                     single = []
                                                   })
                  } 
--------------------------------------------------------------------------

new_york = Calendar {
                        description = Just "NYC", 
                        swiftCode   = Nothing,
                        weekEnd     = [Saturday, Sunday],
                        holidays    = Just (Holidays {
                                                       perpetual = [(12, 24)],
                                                       single = []
                                                     })
                    } 

