-----------------------------------------------------------------------------
--
-- Problem 48
--
-- Self Powers
--
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
--
-----------------------------------------------------------------------------

module Problem48 (
    problem48
) where

takeLast :: Int -> [a] -> [a]
takeLast n list = reverse $ take n $ reverse list

problem48 = takeLast 10 $ show $ sum [x^x | x <- [1..1000]]

