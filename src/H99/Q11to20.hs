module H99.Q11to20
    ( tests11to20
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           H99.Q1to10       (encode)

{-| Problem 11.
  Modify the result of problem 10 in such a way that if an element has no duplicates it is
  simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
-}
data EncodedElem a = Single a | Multiple Int a deriving (Show, Eq)
encodeModified :: (Eq a) => [a] -> [EncodedElem a]
encodeModified xs = map (\(c, x) -> if c > 1 then Multiple c x else Single x) $ encode xs

problem11 :: TestTree
problem11 = testGroup "Problem 11" [ testCase "encodeModified \"aaaabccaadeeee\"" $
                                     encodeModified "aaaabccaadeeee" @?= [Multiple 4 'a',Single 'b',Multiple 2 'c',
                                                                          Multiple 2 'a',Single 'd',Multiple 4 'e']
                                   , testCase "encodeModified []" $
                                     encodeModified ([] :: [Int]) @?= []
                                   ]

tests11to20 :: TestTree
tests11to20 = testGroup "Q11 - 20"
             [ problem11 ]
