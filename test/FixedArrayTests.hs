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
  arr' <- foldM unsafeAdd arr ([0..20] :: [Int])
  values <- FixedArray.foldlM concat [] arr'
  values @?= reverse [0..9]
 where
  concat l _ v = pure (v:l)
  unsafeAdd a b = Vec.unsafeRunUpdateM $ FixedArray.add a b

testDelete :: IO ()
testDelete = do
  arr <- FixedArray.new 10
  arr' <- foldM unsafeAdd arr ([0..9] :: [Int])
  arr'' <- unsafeDelete arr' 1
  arr''' <- unsafeDelete arr'' 10
  values <- FixedArray.foldlM concat [] arr'''
  values @?= reverse [2..9]
  arr'''' <- foldM unsafeDelete arr''' values
  values' <- FixedArray.foldlM concat [] arr''''
  values' @?= []
 where
  concat l i _ = pure (i:l)
  unsafeAdd a b = Vec.unsafeRunUpdateM $ FixedArray.add a b
  unsafeDelete a b = Vec.unsafeRunUpdateM $ FixedArray.delete a b

testInsertAndDelete :: IO ()
testInsertAndDelete = do
  arr <- FixedArray.new 20
  arr' <- foldM unsafeAdd arr ([0..9] :: [Int])
  arr'' <- foldM unsafeDelete arr' [1..5]
  arr''' <- foldM unsafeAdd arr'' [10..14]
  values <- FixedArray.foldlM concat [] arr'''
  values @?= reverse [14, 13, 12, 11, 10, 5, 6, 7, 8, 9]
  show arr''' @?= show arr' -- Test that the capacity is the same.
 where
  concat l _ v = pure (v:l)
  unsafeAdd a b = Vec.unsafeRunUpdateM $ FixedArray.add a b
  unsafeDelete a b = Vec.unsafeRunUpdateM $ FixedArray.delete a b
