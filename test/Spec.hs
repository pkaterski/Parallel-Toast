{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (isJust)

import Types
import Config
import Helper
import Streaming

main :: IO ()
main = hspec $ do
  describe "Operations" $ do
    it "correctly applies Add operation" $
      applyOperation Add 5 3 `shouldBe` Right 8

    it "correctly applies Subtract operation" $
      applyOperation Subtract 5 3 `shouldBe` Right 2

    it "correctly applies Multiply operation" $
      applyOperation Multiply 5 3 `shouldBe` Right 15

    it "correctly applies Divide operation" $
      applyOperation Divide 6 3 `shouldBe` Right 2

    it "returns error on division by zero" $
      applyOperation Divide 5 0 `shouldBe` Left "Division by zero"

    prop "Add is commutative" $
      \x y -> applyOperation Add x y == applyOperation Add y x

    prop "Multiply is commutative" $
      \x y -> applyOperation Multiply x y == applyOperation Multiply y x

  describe "Parsing" $ do
    it "correctly parses a valid number" $
      runConduit (CL.sourceList ["123.45"] .| parseNumber .| CL.consume)
        `shouldReturn` [Right 123.45]

    it "returns error for invalid number" $
      runConduit (CL.sourceList ["abc"] .| parseNumber .| CL.consume)
        `shouldReturn` [Left "Failed to parse number: abc Error: input does not start with a digit"]

    it "correctly parses words" $
      runConduit (CL.sourceList ["hello world\nnew line"] .| parseWords .| CL.consume)
        `shouldReturn` ["hello", "world", "new", "line"]

  describe "Config" $ do
    it "correctly reads a valid config file" $ do
      config <- readConfigFromFile "test/valid_config.json"
      config `shouldSatisfy` isJust

    it "returns Nothing for an invalid config file" $ do
      config <- readConfigFromFile "test/invalid_config.json"
      config `shouldBe` Nothing

    it "correctly splits jobs into batches" $ do
      let config = Config [Job 1 "in1" "out1" [Add], Job 2 "in2" "out2" [Subtract]] 1 "log.txt"
      getBatches config `shouldBe` [[Job 1 "in1" "out1" [Add]], [Job 2 "in2" "out2" [Subtract]]]

