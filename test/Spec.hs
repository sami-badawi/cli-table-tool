import           Control.Exception (evaluate)
import           Test.Hspec

import           SchemaLib
import           Sheet

exampleList :: [String]
exampleList = [
  "+---------------+--------+",
  "|       Sentient|   count|",
  "+---------------+--------+",
  "|          Robot|    1628|",
  "|Other Non-Human|    1738|",
  "|         Monkey|   11258|",
  "|   Space Aliens|  713280|",
  "|      Smart-App| 8295982|",
  "|          Human|43245298|",
  "+---------------+--------+",
  ""]

exampleString = unlines exampleList

example1 :: [String]
example1 = [
  "+---------------+--------+",
  "|       Sentient|   count|",
  "+---------------+--------+",
  "|          Human|43245298|",
  "+---------------+--------+",
  ""]

example1String :: String
example1String = unlines example1

sheet1 :: Sheet
sheet1 = Sheet { mbHeadline = Just ["Sentient", "count"]
    , sheetWidth = 2
    , cells = [ ["Human", "43245298"] ]
    , columnTypes = [StringType,StringType]}

main :: IO ()
main = hspec $ do
  describe "SchemaLib.splitLine" $ do
    it "split a line but retain spaces" $ do
      (splitLine "|      Human|43245298|") `shouldBe` ["","      Human", "43245298",""]

  describe "SchemaLib.parseLine" $ do
    it "split a line but trim spaces" $ do
      (parseLine "|      Human|43245298|") `shouldBe` ["Human", "43245298"]

  describe "SchemaLib.textToLines" $ do
    it "split a line but trim spaces" $ do
      (textToLines "|      Human|43245298|") `shouldBe` [["Human", "43245298"]]

  describe "SchemaLib.textToLines" $ do
    it "split a line but trim spaces" $ do
      (textToLines "|      Human|43245298|") `shouldBe` [["Human", "43245298"]]

  describe "SchemaLib.extractSecondNumber" $ do
    it "split a line but trim spaces" $ do
      extractSecondNumber ["Human", "43245298"] `shouldBe` Just 43245298.0

  describe "SchemaLib.calcSumOfLines" $ do
    it "on one good row should just be the number" $ do
      calcSumOfLines [["Human", "43245298"]] `shouldBe` 43245298.0

  describe "SchemaLib.calcSumOfLines" $ do
    it "on two good row should be the sum number" $ do
      calcSumOfLines [["Robot", "1311"], ["Human", "43245298"]] `shouldBe` (1311.0 + 43245298.0)

  describe "SchemaLib.readSheet" $ do
    it "on two good row should be the sum number" $ do
      (readSheet example1String) `shouldBe` sheet1




