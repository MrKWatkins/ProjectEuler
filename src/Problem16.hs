-----------------------------------------------------------------------------
--
-- Power digit sum
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
-- What is the sum of the digits of the number 2^1000?
--
-----------------------------------------------------------------------------

module Problem16 (
    problem16
) where

import Data.Char

digits :: Integer -> [Integer]
digits n = map (toInteger . digitToInt) $ show n

problem16 = sum . digits $ 2^1000
