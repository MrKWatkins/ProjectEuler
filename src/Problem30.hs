-----------------------------------------------------------------------------
--
-- Digit fifth powers
--
-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of
-- their digits:
--
-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
--
-- As 1 = 1^4 is not a sum it is not included.
--
-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--
-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--
--
-----------------------------------------------------------------------------

module Problem30 (
    problem30
) where

import Core

sumOfPowers :: Integer -> Integer -> Integer
sumOfPowers power n = sum . map (^power) $ digits n

isSumOfPowers :: Integer -> Integer -> Bool
isSumOfPowers power n = n == sumOfPowers power n

isSumOfFifthPowers = isSumOfPowers 5

hasMultipleNonZeroDigits :: Integer -> Bool
hasMultipleNonZeroDigits n = length (filter (/= 0) $ digits n) > 1

-- sumOfPowers 5 999999 = 413343
-- sumOfPowers 5 9999999 = 472392
-- So highest 7 digit number has a sumOfPowers that doesn't reach 7 digits. We can therefore
-- stop by this point. (And probably quite a way before that)

problem30 = sum [x | x <- [0..9999999], isSumOfFifthPowers x, hasMultipleNonZeroDigits x]
