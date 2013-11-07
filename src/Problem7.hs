-----------------------------------------------------------------------------
--
-- Problem 7
--
-- 10001st prime
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10 001st prime number?
--
-----------------------------------------------------------------------------

module Problem7 (
    problem7
) where

-- Rather unefficient I know...
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\x -> n `mod` x /= 0) $ 2:[3,5..n-1]

primes :: [Integer]
primes = filter isPrime $ 2:[3,5..]

nthPrime n = primes !! (n - 1)    -- -1 as primes is zero indexed

problem7 = nthPrime 10001

