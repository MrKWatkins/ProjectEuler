-----------------------------------------------------------------------------
--
-- Module      :  Core
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Core (
    digits,
    digitCount,
    factorial
) where

import Data.Char

digits :: Integer -> [Integer]
digits n = map (toInteger . digitToInt) $ show n

digitCount :: Integer -> Int
digitCount = length . digits

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)
