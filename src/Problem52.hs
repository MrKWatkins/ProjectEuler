-----------------------------------------------------------------------------
--
-- Permuted multiples
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.
--
-----------------------------------------------------------------------------

module Problem52 (
    problem52
) where

import Core
import Data.List

sortDigits :: Integer -> Integer
sortDigits = read . sort . intToString

isPermutedMultiple :: Integer -> Bool
isPermutedMultiple n = all (== sortDigits n) $ map sortDigits multiples
	where
		multiples = map (*n) [2..6]

problem52 = head $ filter isPermutedMultiple [1..]