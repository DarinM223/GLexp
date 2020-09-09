module Main (main) where

import Test.Tasty

import qualified FixedArrayTests as F

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ F.tests
  ]
