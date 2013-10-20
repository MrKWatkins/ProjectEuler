-----------------------------------------------------------------------------
--
-- Problem 5
--
-- Smallest multiple
--
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?
--
-----------------------------------------------------------------------------

module Problem5 (
    problem5
) where

isDivisibleBy :: Integral a => a -> a -> Bool
isDivisibleBy number divisor = number `mod` divisor == 0

dividesInto :: Integral a => a -> a -> Bool
dividesInto = flip isDivisibleBy

-- Brute force approach.
smallestMultipleBruteForce :: (Integral a) => [a] -> a
smallestMultipleBruteForce divisors = head [x | x <- [1..], all (isDivisibleBy x) divisors]

-- Reduces the number of divisors, e.g. if a number is evenly divisible by 10
-- then its also evenly divisible by 5.
dividesAnyOf :: Integral a => a -> [a] -> Bool
dividesAnyOf divisor numbers = any (dividesInto divisor) numbers

reduceDivisors :: Integral a => [a] -> [a]
reduceDivisors divisors = filter (\x -> not $ x `dividesAnyOf` (filter (>x) divisors)) divisors

-- Smarter - step by the size of the smallest divisor.
smallestMultiple :: Integral a => [a] -> a
smallestMultiple divisors = head [x | x <- sequence, all (isDivisibleBy x) divisors]
    where
        smallestDivisor = minimum divisors
        sequence = [smallestDivisor,2*smallestDivisor..]


problem5 = smallestMultiple $ reduceDivisors [1..20]
