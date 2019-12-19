import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Model(
  moveLeftReducerGrid,
  horizontalFlip,
  verticalFlip)

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
  describe "move to left" $ do
    it "should return a correct aggregation to the left" $ do
        moveLeftReducerGrid [
            [2,0,0,0],
            [2,2,0,0],
            [2,2,2,0],
            [2,2,2,2]]
        `shouldBe` [
            [2,0,0,0],
            [4,0,0,0],
            [4,2,0,0],
            [4,4,0,0]]

  describe "move to left 2" $ do
    it "should return a correct aggregation to the left" $ do
        moveLeftReducerGrid [
            [2,0,0,0],
            [4,0,0,0],
            [4,2,0,0],
            [4,4,0,0]]
        `shouldBe` [
            [2,0,0,0],
            [4,0,0,0],
            [4,2,0,0],
            [8,0,0,0]]

  describe "vertical flip" $ do
    it "should flip the grid vertically (number on the left should be on the right)" $ do
        verticalFlip [
            [2,0,0,0],
            [4,0,0,0],
            [4,2,0,0],
            [4,4,0,0]]
        `shouldBe` [
            [0,0,0,2],
            [0,0,0,4],
            [0,0,2,4],
            [0,0,4,4]]

  describe "horizontal flip" $ do
    it "should flip the grid horizontally (number on the top should be on the bottom)" $ do
        horizontalFlip [
            [2,0,0,0],
            [4,0,0,0],
            [4,2,0,0],
            [4,4,0,0]]
        `shouldBe` [
            [4,4,0,0],
            [4,2,0,0],
            [4,0,0,0],
            [2,0,0,0]]