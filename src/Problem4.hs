-----------------------------------------------------------------------------
--
-- Problem 4
--
-- Largest palindrome product
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.
--
-----------------------------------------------------------------------------

module Problem4 (
    problem4
) where


isPalindrome :: Show a => a -> Bool
isPalindrome x = (reverse $ show x) == show x

problem4 = maximum [ x | y <- [1..999], z <- [1..999], let x = y*z, isPalindrome x ]
