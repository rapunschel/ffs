module Number where 
import Data.Foldable

data Number = Zero | Succ Number
  deriving (Show, Read, Eq)
      
instance Enum Number where

  toEnum n 
   | n == 0      = Zero
   | n > 0       = Succ $ toEnum (n - 1)
   | otherwise   = error "negative value"

  fromEnum Zero = 0
  fromEnum (Succ n) = fromEnum n + 1

instance Semigroup Number where

  n <> m = toEnum (fromEnum n + fromEnum m)

instance Monoid Number where

  mempty = Zero

instance Num Number where

  n + m = op (+) n m
  n - m = op (-) n m
  n * m = op (*) n m

  abs = id
  signum = const $ Succ Zero

  fromInteger = toEnum . fromEnum

op :: (Int -> Int -> Int) -> Number -> Number -> Number
op f n m = toEnum $ fromEnum n `f` fromEnum m