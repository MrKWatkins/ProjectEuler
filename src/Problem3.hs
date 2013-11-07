-----------------------------------------------------------------------------
--
-- Largest prime factor
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143?
--
-----------------------------------------------------------------------------

module Problem3 (
    problem3
) where

import Data.Numbers.Primes

primeFactors :: Integer -> [Integer]
primeFactors n = filter (\x -> n `mod` x == 0) $ takeWhile (< max) primes
    where
        max = toInteger . ceiling . sqrt . fromIntegral $ n

problem3 = maximum $ Problem3.primeFactors 600851475143
