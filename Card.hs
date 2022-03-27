module Card where
import Number 

data Color = White | Yellow | Blue | Green | Red 
 deriving (Show, Read, Eq, Enum)

data Card = Card Color Number
 deriving (Show, Read, Eq)

type Deck = Bunch Card

infixl 5 `Comma`
infixl 6 `And`

data Bunch a = a `Comma` (Bunch a) |  a `And` a
 deriving (Eq, Show, Read)

bunch :: a -> a -> [a] -> Bunch a
bunch c0 c1 [] = c0 `And` c1
bunch c0 c1 (c2:cs) = c0 `Comma` bunch c1 c2 cs

--- TEST

c0 = Card White Zero
c1 = Card Red (toEnum 100)
c2 = Card Yellow (toEnum 42)
c3 = Card Red (toEnum 4)
c4 = Card Green (Succ Zero)
c5 = Card Blue (Succ (Succ Zero))
c6 = Card Blue Zero
c7 = Card Green Zero

d0 = c0 `And` c1
d1 = c5 `Comma` (c6 `Comma` (c3 `And` c4))
d2 = c2 `And` c7

deck = bunch c0 c1 [c2,c3,c4,c5,c6,c7]