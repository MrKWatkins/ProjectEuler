-----------------------------------------------------------------------------
--
-- Non-abundant sums
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors
-- of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect
-- number.
--
-- A number n is called deficient if the sum of its proper divisors is less
-- than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant numbers
-- is 24. By mathematical analysis, it can be shown that all integers greater
-- than 28123 can be written as the sum of two abundant numbers. However,
-- this upper limit cannot be reduced any further by analysis even though it
-- is known that the greatest number that cannot be expressed as the sum of
-- two abundant numbers is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the
-- sum of two abundant numbers.
--
-----------------------------------------------------------------------------

module Problem23 (
    problem23
) where

import Data.List
import Divisors
import qualified Data.IntSet as I

sumOfProperDivisorsMatches :: Integral a => (a -> a -> Bool) -> a -> Bool
sumOfProperDivisorsMatches predicate n = predicate (sum $ properDivisors n) n

isPerfect :: Integral a => a -> Bool
isPerfect = sumOfProperDivisorsMatches (==)

isDeficient :: Integral a => a -> Bool
isDeficient = sumOfProperDivisorsMatches (<)

isAbundant :: Integral a => a -> Bool
isAbundant = sumOfProperDivisorsMatches (>)

-- All abundants up to the 28123 limit.
abundants :: [Int]
abundants = filter isAbundant [1..28123]


problem23 = sum $ filter notSumOfTwoAbundants [1..28123]
	where
		-- A set of all the possible sums of two abundant numbers up to the 28123 limit.
		sumOfTwoAbundants = I.fromList [x + y | x <- abundants, y <- abundants, y >= x]
		-- Determines if a number is not the sum of two abundants by looking it up in the set of sums.
		notSumOfTwoAbundants x = I.notMember x sumOfTwoAbundants