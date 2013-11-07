-----------------------------------------------------------------------------
--
-- Double-base palindromes
--
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both
-- bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic
-- in base 10 and base 2. (Please note that the palindromic number, in either
-- base, may not include leading zeros.)
--
-----------------------------------------------------------------------------

module Problem36 (
    problem36
) where

import Core

isPalindrome s = (reverse s) == s

isDecimalPalindrome n = isPalindrome $ intToString n
isBinaryPalindrome n = isPalindrome $ intToBinaryString n



problem36 = sum [x | x <- [1..999999], isDecimalPalindrome x, isBinaryPalindrome x]

