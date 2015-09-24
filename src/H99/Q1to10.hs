module H99.Q1to10
    ( tests1to10
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit as HU

{-| Problem 1.
  Find the last element of a list.
-}
myLast :: [a] -> a
myLast [] = error "Empty list has no last element"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

problem1 :: TestTree
problem1 = testGroup "Problem 1" [ testCase "myLast [1,2,3,4]" $
                                   myLast [1,2,3,4] @?= 4
                                 , testCase "myLast ['x','y','z']" $
                                   myLast "xyz" @?= 'z'
                                 ]

tests1to10 :: TestTree
tests1to10 = testGroup "Q1 - 10"
             [ problem1 ]
