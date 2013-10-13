-----------------------------------------------------------------------------
--
-- Problem 41
--
-- Pandigital prime
-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.
--
-- What is the largest n-digit pandigital prime that exists?
--
-----------------------------------------------------------------------------

module Problem41 (
    problem41
) where

import Data.Char
import Data.List
import Data.Numbers.Primes

panDigitals :: Int -> [Int]
panDigitals n = map (read) $ permutations digits
    where
        digits = map (intToDigit) [1..n]

reverseOrderedPanDigitals = concat $ map (reverse . sort . panDigitals) [9,8..1]

problem41 = head [x | x <- reverseOrderedPanDigitals, isPrime x]
