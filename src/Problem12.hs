-----------------------------------------------------------------------------
--
-- Highly divisible triangular number
-- The sequence of triangle numbers is generated by adding the natural numbers.
-- So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
-- ten terms would be:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- Let us list the factors of the first seven triangle numbers:
--
--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28
--
-- We can see that 28 is the first triangle number to have over five divisors.
--
-- What is the value of the first triangle number to have over five hundred divisors?
--
-----------------------------------------------------------------------------

module Problem12 (
    problem12
) where

import Data.List
import Data.Numbers.Primes

triangularNumbers :: [Integer]
triangularNumbers = scanl1 (+) [1..]

-- From http://www.vitutor.com/arithmetic/divisibility/divisors.html we get
-- the prime factorization in the form p1^e1 + p2 ^e2... From this the number
-- of divisors is (e1 + 1) * (e2 + 1) *...
-- primeFactors gives a list like [2,2,2,5,5,5,5] (for 5000)
-- Grouping gives [[2,2,2],[5,5,5,5]]. Taking the lengths gives us the e numbers.
-- genericLength needed to keep Integral a; length has type Int, genericLength type Integral.

divisors :: Integral a => a -> a
divisors n = product $ map ((+1) . genericLength) $ group $ primeFactors n

problem12 = head $ [t | t <- triangularNumbers, (divisors t) > 500]