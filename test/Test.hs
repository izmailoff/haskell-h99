module Main where

import           H99.Q1to10  (tests1to10)
import           H99.Q11to20  (tests11to20)

import           Test.Tasty

tests :: TestTree
tests = testGroup "H99"
        [ tests1to10, tests11to20 ]

main :: IO ()
main = defaultMain tests
