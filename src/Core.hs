-----------------------------------------------------------------------------
--
-- Module      :  Core
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Core (
	intToStringInBase,
	intToString,
	intToBinaryString,
	digitsInBase,
	digits,
	binaryDigits,
	digitCountInBase,
	digitCount,
	binaryDigitCount,
	factorial
) where

import Numeric
import Data.Char

intToStringInBase :: (Integral a, Show a) => a -> a -> String
intToStringInBase base n = showIntAtBase base intToDigit n ""

intToString  :: (Integral a, Show a) => a -> String
intToString = intToStringInBase 10

intToBinaryString  :: (Integral a, Show a) => a -> String
intToBinaryString = intToStringInBase 2

digitsInBase :: (Integral a, Show a) => a -> a -> [Integer]
digitsInBase base n = map (toInteger . digitToInt) $ intToStringInBase base n

digits :: (Integral a, Show a) => a -> [Integer]
digits = digitsInBase 10

binaryDigits :: (Integral a, Show a) => a -> [Integer]
binaryDigits = digitsInBase 2

digitCountInBase :: (Integral a, Show a) => a -> a -> Int
digitCountInBase base n = length $ intToStringInBase base n

digitCount :: (Integral a, Show a) => a -> Int
digitCount = digitCountInBase 10

binaryDigitCount :: (Integral a, Show a) => a -> Int
binaryDigitCount = digitCountInBase 2

intToExpFormatPres :: (Integral a, Show a) => Int -> a -> String
intToExpFormatPres p x = head digits : decimalPlaces ++ "E+" ++ intToString exponent
	where
		digits = intToString x
		exponent = length digits - 1
		paddedTail = tail digits ++ repeat '0'
		decimalPlaces = if p == 0 then "" else '.' : take p paddedTail

intToExpFormat :: (Integral a, Show a) => a -> String
intToExpFormat = intToExpFormatPres 2

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)