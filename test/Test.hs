module Main where

import           H99.Q1to10  (tests1to10)

import           Test.Tasty

tests :: TestTree
tests = testGroup "H99"
        [ tests1to10 ]

main :: IO ()
main = defaultMain tests
