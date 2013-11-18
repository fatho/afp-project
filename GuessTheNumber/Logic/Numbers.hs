module Logic.Numbers where

import Prelude

showClassicRoman :: (Show a, Integral a) => a -> String
showClassicRoman x 
  | x <  0 = show x ++ " (non scio hunc numerum)"
  | x == 0 = "nihil"
  | otherwise = go x
  where
    go x
      | x >= 1000 = 'M': go (x - 1000)
      | x >= 500 = 'D': go (x - 500)
      | x >= 100 = 'C': go (x - 100)
      | x >= 50 = 'L': go (x - 50)
      | x >= 10 = 'X': go (x - 10)
      | x >= 5 = 'V': go (x - 5)
      | x >= 1 = 'I': go (x - 1)
      | x == 0 = ""

showModernRoman :: (Show a, Integral a) => a -> String
showModernRoman x 
  | x <  0 = show x ++ " (non scio hunc numerum)"
  | x == 0 = "nihil"
  | otherwise = go x
  where
    go x
      | x >= 1000 = 'M': go (x - 1000)
      | x >= 900 = "CM" ++ go (x - 900)
      | x >= 500 = 'D': go (x - 500)
      | x >= 400 = "CD" ++ go (x - 400)
      | x >= 100 = 'C': go (x - 100)
      | x >= 90 = "XC" ++ go (x - 90)
      | x >= 50 = 'L': go (x - 50)
      | x >= 40 = "XL" ++ go (x - 40)
      | x >= 10 = 'X': go (x - 10)
      | x >= 9 = "IX" ++ go (x - 9)
      | x >= 5 = 'V': go (x - 5)
      | x >= 4 = "IV" ++ go (x - 4)
      | x >= 1 = 'I': go (x - 1)
      | x == 0 = ""