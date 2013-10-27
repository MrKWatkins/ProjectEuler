-----------------------------------------------------------------------------
--
-- Special Pythagorean triplet
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--
-----------------------------------------------------------------------------

module Problem9 (
    problem9
) where

isTriplet :: Integral a => a -> a -> a -> Bool
isTriplet a b c = a^2 + b^2 == c^2

problem9 = head [a*b*c | a <- [1..1000], b <- [(a+1)..1000], c <- [(b+1)..1000], a+b+c == 1000, isTriplet a b c]
