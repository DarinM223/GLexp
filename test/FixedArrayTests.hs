module FixedArrayTests (tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

import qualified Engine.FixedArray as FixedArray

tests :: TestTree
tests = testGroup "Fixed array tests"
  [ testCase "Test insert" testInsert
  , testCase "Test delete" testDelete
  , testCase "Test insert fills deleted spots" testInsertAndDelete
  ]

testInsert :: IO ()
testInsert = do
  arr <- FixedArray.new 10
  arr' <- foldM FixedArray.add arr ([0..20] :: [Int])
  values <- FixedArray.foldlM concat [] arr'
  values @?= reverse [0..9]
 where concat l _ v = pure (v:l)

testDelete :: IO ()
testDelete = do
  arr <- FixedArray.new 10
  arr' <- foldM FixedArray.add arr ([0..9] :: [Int])
  arr'' <- FixedArray.delete arr' 1
  arr''' <- FixedArray.delete arr'' 10
  values <- FixedArray.foldlM concat [] arr'''
  values @?= reverse [2..9]
  arr'''' <- foldM FixedArray.delete arr''' values
  values' <- FixedArray.foldlM concat [] arr''''
  values' @?= []
 where concat l i _ = pure (i:l)

testInsertAndDelete :: IO ()
testInsertAndDelete = do
  arr <- FixedArray.new 20
  arr' <- foldM FixedArray.add arr ([0..9] :: [Int])
  arr'' <- foldM FixedArray.delete arr' [1..5]
  arr''' <- foldM FixedArray.add arr'' [10..14]
  values <- FixedArray.foldlM concat [] arr'''
  values @?= reverse [14, 13, 12, 11, 10, 5, 6, 7, 8, 9]
  show arr''' @?= show arr' -- Test that the capacity is the same.
 where concat l _ v = pure (v:l)
