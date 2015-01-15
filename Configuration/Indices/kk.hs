{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -XScopedTypeVariables #-}

import Utils.MyJSON
import Utils.MyUtils
import Data.Data
import Data.Typeable
import Data.Maybe
import Data.Char

data KK1 = KK1{a1::KK2, b1::Maybe KK2} deriving (Eq, Show, Data, Typeable)
data KK2 = KK2{a2::KK3} deriving (Eq, Show, Data, Typeable)
data KK3 = KK3{a3::String} deriving (Eq, Show, Data, Typeable)

kk1 = KK1 {
           a1 = KK2 {
                        a2 = KK3{a3 = "hola"}
                    },
           b1 = Nothing
          }  
          
prueba = kk1 {a1 = (a1 kk1) {a2 = ((a2 . a1) kk1) {a3 = "adios"}}}



prueba2 = kk1 {a1 = kk2}
kk2 = (a1 kk1) {a2 = kk3}
kk3 = ((a2 . a1) kk1) {a3 = "adios"}

char :: Typeable a => a -> String
char x = case cast x of
                  Just (x :: Char) -> show x
                  Nothing -> "unknown"

kk = gmapT
     (\d ->
        case cast d of
          Nothing -> d
          Just x ->
            fromJust (cast (if isUpper x then '!' else x)))
     (Foo 4 'A')
     
data Foo = Foo Int Char deriving (Data,Typeable,Show)     