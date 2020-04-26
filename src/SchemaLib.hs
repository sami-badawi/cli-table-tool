module SchemaLib
where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Options.Applicative
import           Text.Read

-------------------------------------- Types -----------------------------------

type Field = String -- ^ might change to Text
type MulitLine = Field
type TextLine = Field
type Fields = [String]
type Cells = [Fields]

data ColumnType = EmptyType [()] | StringType Fields | DoubleType [Double] | IntType [Int] deriving (Eq, Show)

data Config = Config
  {
    filename :: Field -- ^ filename
  , code :: Bool -- ^ output as code
  , quote :: Bool -- ^ quote all
  , percent :: Bool -- ^ calculate percent of numerical fields
  } deriving (Show)

-------------------------------------- Text Util -------------------------------

trim :: Field -> Field
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

quoteCell :: Field -> Field
quoteCell field = "\"" ++ field ++ "\""

-------------------------------------- Extract ---------------------------------

mbDoubleList :: Fields -> Maybe [Double]
mbDoubleList fields = sequence $ map (readMaybe :: Field -> Maybe Double) fields

mbIntList :: Fields -> Maybe [Int]
mbIntList fields = sequence $ map (readMaybe :: Field -> Maybe Int) fields

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

lineToCsvLine :: Field -> Field
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


-------------------------------------- Commandline -----------------------------

sample :: Parser Config
sample = Config
     <$> strOption
         ( short 'i'
        <> long "input"
        <> metavar "INPUT"
        <> help "Filename to read" )
     <*> switch
         ( short 'c'
        <> long "code"
        <> help "Output for code" )
     <*> switch
         ( short 'q'
        <> long "quote"
        <> help "quote all in output" )
     <*> switch
         ( short 'p'
        <> long "percent"
        <> help "Output percentage for numerical column as seperate column" )

opts :: ParserInfo Config
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Extract from a fix width table and turn it into csv possibly for code"
  <> header "cli-table-tool - Haskell based table extractor util" )
