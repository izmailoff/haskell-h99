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

{-| Problem 2.
  Find the last but one element of a list.
-}
myButLast :: [a] -> a
myButLast [] = error "This list has no penultimate element"
myButLast [x] = error "This list has no penultimate element"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

problem2 :: TestTree
problem2 = testGroup "Problem 2" [ testCase "myButLast [1,2,3,4]" $
                                   myButLast [1,2,3,4] @?= 3
                                 , testCase "myButLast ['a'..'z']" $
                                   myButLast ['a'..'z'] @?= 'y'
                                 ]

tests1to10 :: TestTree
tests1to10 = testGroup "Q1 - 10"
             [ problem1, problem2 ]
