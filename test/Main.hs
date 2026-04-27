module Main (main) where

import Test.QuickCheck

import ProbFunc


main :: IO ()
main = quickCheck prop
