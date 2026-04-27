module Main (main) where

import Test.QuickCheck

import ProbFunc


prop :: Int -> Bool
prop x = ((\y -> y) x) == x


main :: IO ()
main = quickCheck prop
