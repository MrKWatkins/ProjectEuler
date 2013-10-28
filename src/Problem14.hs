-----------------------------------------------------------------------------
--
-- Longest Collatz sequence
--
-- The following iterative sequence is defined for the set of positive integers:
--
-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers
-- finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.
--
-----------------------------------------------------------------------------

module Problem14 (
    problem14
) where

import Data.List
import Data.Ord

collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence n
    | even n = n : collatzSequence (truncate (fromIntegral (n) / 2))
    | otherwise = n : collatzSequence (3 * n + 1)

collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n
    | even n = 1 + collatzLength (truncate (fromIntegral (n) / 2))
    | otherwise = 1 + collatzLength (3 * n + 1)

problem14 :: Integer
problem14 = fst $ maximumBy (comparing snd) $ map (\x -> (x, collatzLength x)) [1..999999]
