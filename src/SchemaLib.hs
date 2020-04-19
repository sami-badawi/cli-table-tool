module SchemaLib
where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Read

-------------------------------------- Types -----------------------------------

type MulitLine = String
type TextLine = String
type Fields = [String]
type Cells = [Fields]

data ColumnType = EmptyType | StringType | DoubleType | IntType  deriving (Bounded, Enum, Eq, Ord, Show)

-------------------------------------- Text Util -------------------------------

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

splitLine :: TextLine -> Fields
splitLine = splitOn "|"

removeEmptyEnds :: Fields -> Fields
removeEmptyEnds = dropWhileEnd null . dropWhile null

-- | combine by seperating wiht comma
fieldsToCsvLine :: Fields -> TextLine
fieldsToCsvLine =  intercalate ","

-- | make every line in Sheet the same width
makeLineGivenWidth :: Int -> Fields -> Fields
makeLineGivenWidth width fields
  | (length fields) > width = take width fields
  | otherwise = fields ++ (replicate (width - (length fields)) "")

-------------------------------------- Extract ---------------------------------

mbDoubleList :: Fields -> Maybe [Double]
mbDoubleList fields = sequence $ map (readMaybe :: String -> Maybe Double) fields

mbIntList :: Fields -> Maybe [Int]
mbIntList fields = sequence $ map (readMaybe :: String -> Maybe Int) fields

-------------------------------------- Calculate -------------------------------

extractSecondNumber :: (Read a) => Fields -> Maybe a
extractSecondNumber (_ : second : []) = readMaybe second
extractSecondNumber _                 = Nothing

calcSumOfLines :: Cells -> Double
calcSumOfLines lines = res
    where
      maybeNumber = [(extractSecondNumber line) | line <- lines]
      numbers = catMaybes maybeNumber
      res = sum numbers

-------------------------------------- Parse -----------------------------------

parseLine :: TextLine -> Fields
parseLine line = removeEmptyEnds full
    where
        full = [trim field | field <- splitLine line]

lineToCsvLine :: String -> String
lineToCsvLine line = fieldsToCsvLine $ parseLine line

isGoodLine :: Fields -> Bool
isGoodLine line = (length line) >= 2

filterBadLines :: Cells -> Cells
filterBadLines = filter isGoodLine


textToLines :: MulitLine -> Cells
textToLines text = goodLines
    where
        stringLines = lines text
        textLines = map parseLine stringLines
        goodLines = filterBadLines textLines

