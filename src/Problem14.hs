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
import qualified Data.Map.Strict as Map
import Control.Monad.State

collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence n
    | even n = n : collatzSequence (n `div` 2)
    | otherwise = n : collatzSequence (3 * n + 1)

collatzLength :: Integer -> Integer
collatzLength 1 = 1
collatzLength n
    | even n = 1 + collatzLength (n `div` 2)
    | otherwise = 1 + collatzLength (3 * n + 1)

type Cache = Map.Map Integer Integer

nextCollatz :: Integer -> Integer
nextCollatz n
    | even n = n `div` 2
    | otherwise = 3 * n + 1

cachedCollatzLength :: Integer -> State Cache Integer
cachedCollatzLength 1 = return 1
cachedCollatzLength n = do
  cache <- get
  case Map.lookup n cache of
    Nothing -> do
        let next = nextCollatz n
        lengthOfNext <- cachedCollatzLength next
        put $ Map.insert next lengthOfNext cache
        return $ lengthOfNext + 1
    Just length -> return length

trackMax currentMax next = do
    lengthOfNext <- cachedCollatzLength next
    return $ maximumBy (comparing snd) [currentMax, (next, lengthOfNext)]

maxCollatzLength :: [Integer] -> (Integer, Integer)
maxCollatzLength xs = flip evalState Map.empty $ do
  foldM trackMax (1, 1) xs

problem14BruteForce :: [Integer] -> Integer
problem14BruteForce xs = fst $ maximumBy (comparing snd) $ map (\x -> (x, collatzLength x)) xs

problem14Cached :: [Integer] -> Integer
problem14Cached = fst . maxCollatzLength

problem14 = problem14BruteForce [1..99999]
