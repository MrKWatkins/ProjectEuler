-----------------------------------------------------------------------------
--
-- Module      :  Fibonacci
-- Copyright   :  Kevin Watkins
-- License     :  AllRightsReserved
--
-----------------------------------------------------------------------------

module Fibonacci (
    fibSeries,
    genFibSeries,
    fib,
    genFib
) where

genFibSeries :: Num a => a -> a -> [a]
genFibSeries f1 f2 = f1 : f2 : next (genFibSeries f1 f2)
  where
    next (fn_2 : t@(fn_1:_)) = (fn_2 + fn_1) : next t

fibSeries = genFibSeries 0 1

genFib f1 f2 n = genFibSeries f1 f2 !! n

fib n = fibSeries !! n
