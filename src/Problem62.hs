-----------------------------------------------------------------------------
--
-- Cubic permutations
--
-- The cube, 41063625 (345^3), can be permuted to produce two other cubes: 
-- 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest 
-- cube which has exactly three permutations of its digits which are also cube.
--
-- Find the smallest cube for which exactly five permutations of its digits are
-- cube.
--
-----------------------------------------------------------------------------

module Problem62 (
    problem62
) where

import Core
import Data.List
import Data.Maybe

problem62 = n ^ 3
	where
		cubes = [x^3 | x <- [1..10000]]
		digitSortedCubes = map (sort . intToString) cubes
		fiveCubes = filter ((==5) . length) $ group $ sort digitSortedCubes
		firstFiveCube = head $ head fiveCubes
		indexOfFirstFiveCube = elemIndex firstFiveCube digitSortedCubes
		n = 1 + (fromIntegral . fromJust $ indexOfFirstFiveCube)