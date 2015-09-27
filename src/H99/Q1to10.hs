module H99.Q1to10
    ( tests1to10
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit as HU
--import           Test.HUnit.Tools (assertRaises)

{-| Problem 1.
  Find the last element of a list.
-}
myLast :: [a] -> a
myLast [] = error "No such element"
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
myButLast [] = error "No such element"
myButLast [x] = error "No such element"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

problem2 :: TestTree
problem2 = testGroup "Problem 2" [ testCase "myButLast [1,2,3,4]" $
                                   myButLast [1,2,3,4] @?= 3
                                 , testCase "myButLast ['a'..'z']" $
                                   myButLast ['a'..'z'] @?= 'y'
                                 --, testCase "myButLast []" $
                                 --  assertRaises "should throw" (Exception "No such element") $ evaluate myButLast []
                                 ]

{-| Problem 3.
  Find the K'th element of a list. The first element in the list is number 1.
-}
elementAt :: [a] -> Int -> a
elementAt [] _ = error "No such element"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

problem3 :: TestTree
problem3 = testGroup "Problem 3" [ testCase "elementAt [1,2,3] 2" $
                                   elementAt [1,2,3] 2 @?= 2
                                 , testCase "elementAt \"haskell\" 5" $
                                   elementAt "haskell" 5 @?= 'e'
                                  ]

{-| Problem 4.
  Find the number of elements of a list.
-}
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

problem4 :: TestTree
problem4 = testGroup "Problem 4" [ testCase "myLength [123, 456, 789]" $
                                   myLength [123, 456, 789] @?= 3
                                 , testCase "myLength \"Hello, world!\"" $
                                   myLength "Hello, world!" @?= 13
                                  ]

{-| Problem 5.
  Reverse a list.
-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

problem5 :: TestTree
problem5 = testGroup "Problem 4" [ testCase "myReverse \"A man, a plan, a canal, panama!\"" $
                                   myReverse "A man, a plan, a canal, panama!" @?= "!amanap ,lanac a ,nalp a ,nam A"
                                 , testCase "myReverse [1,2,3,4]" $
                                   myReverse [1,2,3,4] @?= [4,3,2,1]
                                  ]

{-| Problem 6.
  Find the number of elements of a list.
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

problem6 :: TestTree
problem6 = testGroup "Problem 6" [ testCase "isPalindrome [1,2,3]" $
                                   isPalindrome [1,2,3] @?= False
                                 , testCase "isPalindrome \"madamimadam\"" $
                                   isPalindrome "madamimadam" @?= True
                                 , testCase "isPalindrome [1,2,4,8,16,8,4,2,1]" $
                                   isPalindrome [1,2,4,8,16,8,4,2,1] @?= True
                                 , testCase "isPalindrome []" $
                                   isPalindrome ([] :: [Int]) @?= True
                                 , testCase "isPalindrome [1]" $
                                   isPalindrome [1] @?= True
                                  ]

tests1to10 :: TestTree
tests1to10 = testGroup "Q1 - 10"
             [ problem1, problem2, problem3, problem4, problem5,
               problem6 ]
