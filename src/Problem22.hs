-----------------------------------------------------------------------------
--
-- Problem 22
--
-- Names scores
--
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
-- obtain a score of 938 × 53 = 49714.
--
--  What is the total of all the name scores in the file?
--
-----------------------------------------------------------------------------

module Problem22 (
    problem22
) where

import System.IO
import Data.Char
import Data.List
import qualified Data.Text as T

charScore :: Char -> Int
charScore c = ord c - ord 'A' + 1

stringScore :: String -> Int
stringScore s = sum $ map charScore s

scoreNames :: [String] -> Int
scoreNames names = 
	sum 
	$ zipWith (*) [1..] -- Pair up scores with their order and multiply order by score.
	$ map stringScore -- Calculate scores for names.
	$ sort names -- Sort names alphabetically.

parseNames :: String -> [String]
parseNames namesString = map T.unpack names
	where
		names = map stripQuotes namesWithQuotes
		namesWithQuotes = T.splitOn (T.pack ",") namesText
		namesText = T.pack namesString
		stripQuotes = T.dropAround isQuote
		isQuote c = c == '"'

problem22 :: IO ()
problem22 = do
   namesString <- readFile "Problem 22 - Names.txt"
   let names = parseNames namesString
   print (scoreNames names)