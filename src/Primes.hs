-----------------------------------------------------------------------------
--
-- Module      :  Primes
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Primes (
    isPrime,
    primes
) where

-- Rather unefficient I know...
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\x -> n `mod` x /= 0) $ 2:[3,5..n-1]

primes :: [Integer]
primes = filter isPrime $ 2:[3,5..]
