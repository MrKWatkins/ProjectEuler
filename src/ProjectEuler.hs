-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Text.Printf
import Control.Exception
import System.CPUTime

import Problem36

-- Timing based on http://www.haskell.org/haskellwiki/Timing_computations.

main = do
    startedAt <- getCPUTime
    putStr "Answer: "

    putStrLn $ show problem36

    finishedAt <- getCPUTime
    let diff = (fromIntegral (finishedAt - startedAt)) / (10^12)
    printf "Time taken: %0.4fs\n" (diff :: Double)

