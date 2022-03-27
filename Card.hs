{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}

module Card where
import Number 

data Color = White | Yellow | Blue | Green | Red 
 deriving (Show, Read, Eq, Enum)

data Card = Card Color Number
 deriving (Show, Read, Eq)

type Deck = Bunch Card

data Bunch a = Nope | Bunch a a
 deriving (Eq, Show, Read)


class Fuck a b where

   (#@!) :: a -> b -> b
   nope :: b

class Fuck a b => FFS a b where

   ffs :: Fuck a b => [a] -> b
   ffs [] = nope :: b
   ffs (x:xs) = x #@! (ffs xs)  








