{-# LANGUAGE TupleSections #-}
module FixedArrayTests (tests) where

import Control.Monad (foldM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Engine.FixedArray as FixedArray
import qualified Engine.Vec as Vec

tests :: TestTree
tests = testGroup "Fixed array tests"
  [ testCase "Test insert" testInsert
  , testCase "Test delete" testDelete
  , testCase "Test insert fills deleted spots" testInsertAndDelete
  ]

testInsert :: IO ()
testInsert = do
  arr <- FixedArray.new 10
  values <- Vec.unsafeRunUpdateM $ do
    arr' <- foldM FixedArray.add arr ([0..20] :: [Int])
    FixedArray.foldlM concat [] arr'
  values @?= reverse [0..9]
 where concat l _ v = pure (v:l)

testDelete :: IO ()
testDelete = do
  arr <- FixedArray.new 10
  (values, values') <- Vec.unsafeRunUpdateM $ do
    arr' <- foldM FixedArray.add arr ([0..9] :: [Int])
    arr'' <- FixedArray.delete arr' 1
    arr''' <- FixedArray.delete arr'' 10
    values <- FixedArray.foldlM concat [] arr'''
    arr'''' <- foldM FixedArray.delete arr''' values
    values' <- FixedArray.foldlM concat [] arr''''
    return (values, values')
  values @?= reverse [2..9]
  values' @?= []
 where concat l i _ = pure (i:l)

testInsertAndDelete :: IO ()
testInsertAndDelete = do
  arr <- FixedArray.new 20
  (capacity1, capacity2, values) <- Vec.unsafeRunUpdateM $ do
    arr' <- foldM FixedArray.add arr ([0..9] :: [Int])
    arr'' <- foldM FixedArray.delete arr' [1..5]
    arr''' <- foldM FixedArray.add arr'' [10..14]
    (show arr', show arr''',) <$> FixedArray.foldlM concat [] arr'''
  values @?= reverse [14, 13, 12, 11, 10, 5, 6, 7, 8, 9]
  capacity1 @?= capacity2 -- Test that the capacity is the same.
 where concat l _ v = pure (v:l)
