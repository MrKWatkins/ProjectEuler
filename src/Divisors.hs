-----------------------------------------------------------------------------
--
-- Module      :  Divisors
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Divisors (
	properDivisors,
	sumOfProperDivisors
) where

import Math.NumberTheory.Powers.Squares

-- Slow version - count up to n/2 checking all the numbers on the way.
-- properDivisors :: Integral a => a -> [a]
-- properDivisors n = [x | x <- [1..(n+1) `div` 2], n `mod` x == 0]

-- Faster version - count up to root n checking the numbers on the way, adding both the number and
-- the one it divides n by at the same time. (Unless the number is a perfect square, in which case
-- we only want to add it once)
properDivisors :: Integral a => a -> [a]
properDivisors n 
	-- No error checking!
	| n <= 3		= [1]
	| otherwise = rec [1] 2
		where
			root = integerSquareRoot' n
			perfectSquare = root*root == n
			-- integerSquareRoot is the highest square <= n. If it's not a perfect square we therefore
			-- need to go one higher, e.g. for 6 we need to stop at 3 (Which won't be included) not 2.
			-- We could change the n mod x guard to check if n div x == x and only add once if it is
			-- but that would involve a check for each divisor rather than a single one for the limit.
			limit = if perfectSquare then root else root + 1
			rec list x
				| x == limit 		= if perfectSquare then limit:list else list
				| n `mod` x == 0	= rec (x:(n `div` x):list) (x+1)
				| otherwise			= rec list (x+1)

sumOfProperDivisors :: Integral a => a -> a
sumOfProperDivisors = sum . properDivisors