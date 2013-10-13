-----------------------------------------------------------------------------
--
-- Problem 6
--
-- Sum square difference
--
-- The sum of the squares of the first ten natural numbers is,
--
-- 12 + 22 + ... + 102 = 385
--
-- The square of the sum of the first ten natural numbers is,
--
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025
--
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025 - 385 = 2640.
--
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.
--
-----------------------------------------------------------------------------

module Problem6 (
    problem6
) where

sumOfSquares :: (Integral a) => [a] -> a
sumOfSquares = sum . map (^2)

squareOfSum :: (Integral a) => [a] -> a
squareOfSum numbers = (sum numbers) ^ 2

sumSquareDifference :: (Integral a) => [a] -> a
sumSquareDifference numbers = (squareOfSum numbers) - (sumOfSquares numbers)

problem6 :: Integer
problem6 = sumSquareDifference [1..100]
