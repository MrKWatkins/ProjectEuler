-----------------------------------------------------------------------------
--
-- Circular primes
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?
--
-----------------------------------------------------------------------------

module Problem35 (
    problem35
) where

import Core
import Data.Numbers.Primes

rotations :: Integer -> [Integer]
rotations n = n : map (read . swap) [splitAt x d | x <- [1..l-1]]
	where
		d = intToString n
		l = length d
		swap (x,y) = y ++ x

allRotationsPrime :: Integer -> Bool
allRotationsPrime n = all isPrime $ rotations n

problem35 = length . filter allRotationsPrime $ takeWhile (<1000000) primes