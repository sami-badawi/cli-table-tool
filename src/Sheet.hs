module Sheet
where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Text.Read

import           SchemaLib

-------------------------------------- Types -----------------------------------

-- | Sheet is collection of features that would have been in an object in OOP
data Sheet = Sheet {
  mbHeadline  :: Maybe Fields -- ^ some sheets has a headline some do not
, sheetWidth  :: Int -- ^ there is a standard width
, cells       :: Cells -- ^ 2D grid of Strings not assumed to have good lenght
, columnTypes :: [ColumnType] -- ^ What is the type of the columns
} deriving (Eq, Show)


-------------------------------------- Sheet -----------------------------------

readSheet :: MulitLine -> Sheet
readSheet text = sheet
  where
    goodLines = textToLines text
    sheetWidth1 = maximum [ length line | line <- goodLines]
    mbHeadline = listToMaybe goodLines
    columnTypes = replicate sheetWidth1 StringType
    sheet = Sheet { mbHeadline = mbHeadline
    , sheetWidth = sheetWidth1
    , cells = drop 1 goodLines
    , columnTypes = columnTypes}


-- | make every line in Sheet the same width
makeSameWidth :: Sheet -> Int -> Sheet
makeSameWidth sheet width = res
  where
    fixLine = makeLineGivenWidth width
    fixedLines = [ fixLine line | line <- (cells sheet) ]
    res = sheet {cells = fixedLines, sheetWidth = width}

-- | unabitious first
refineSheet :: Sheet -> Sheet
refineSheet sheet = res
  where
    res = sheet

toCsv :: Sheet -> MulitLine
toCsv sheet = res
    where
      allLines = (maybeToList (mbHeadline sheet)) ++ (cells sheet)
      parsedLines = map fieldsToCsvLine allLines
      res = unlines parsedLines

-------------------------------------- Main ------------------------------------

handleText :: MulitLine -> MulitLine
handleText text = res
  where
    sheet = readSheet text
    res = toCsv sheet

