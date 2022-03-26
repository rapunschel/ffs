module Number where 

data Number = Zero | Succ Number
  deriving (Show, Read, Eq)
      
instance Enum Number where

  toEnum n 
   | n == 0      = Zero
   | n > 0       = Succ $ (toEnum (n - 1))
   | otherwise   = error "negate value"

  fromEnum Zero = 0
  fromEnum (Succ n) = fromEnum n + 1
