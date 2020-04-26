module Sheet
where

import           Control.Applicative
import           Data.List
import           Data.Maybe

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
    mbHeadlineF = listToMaybe goodLines
    cellsF = drop 1 goodLines
    columns = transpose cellsF
    columnTypesF = map refineColumn columns 
    sheet = Sheet { mbHeadline = mbHeadlineF
    , sheetWidth = sheetWidth1
    , cells = cellsF
    , columnTypes = columnTypesF}


-- | make every line in Sheet the same width
makeSameWidth :: Sheet -> Int -> Sheet
makeSameWidth sheet width = res
  where
    fixLine = makeLineGivenWidth width
    fixedLines = [ fixLine line | line <- (cells sheet) ]
    res = sheet {cells = fixedLines, sheetWidth = width}

refineColumn :: Fields -> ColumnType
refineColumn fields = res
  where
    resInt = mbIntList fields
    resDouble = mbDoubleList fields
    mbRes :: Maybe ColumnType
    mbRes = (IntType <$> resInt) <|> (DoubleType <$> resDouble)
    res = fromMaybe (StringType fields) mbRes

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

toCode :: Sheet -> MulitLine
toCode sheet = res
    where
      isQuoteColumnType (StringType _) = True
      isQuoteColumnType  _             = False
      quoteField = map isQuoteColumnType $ columnTypes sheet
      quoteLine fields = processedFields
        where
          zippedLine = zip fields quoteField
          quotePair (field, True)  = quoteCell field
          quotePair (field, False) = field
          processedFields = map quotePair zippedLine
      allLines = (maybeToList (mbHeadline sheet)) ++ (map quoteLine (cells sheet))
      parsedLines = map fieldsToCsvLine allLines
      res = unlines parsedLines

toQuotedCsv :: Sheet -> MulitLine
toQuotedCsv sheet = res
    where
      quoteLine = map quoteCell
      allLines = (maybeToList (mbHeadline sheet)) ++ (cells sheet)
      allQuotedLines = map quoteLine allLines
      parsedLines = map fieldsToCsvLine allQuotedLines
      res = unlines parsedLines
-------------------------------------- Percent calculation ---------------------

columnToFields :: ColumnType -> Fields
columnToFields (DoubleType fields) = map show fields
columnToFields (EmptyType fields) = map show fields
columnToFields (IntType fields) = map show fields
columnToFields (StringType fields) = fields



columnTypesToCells :: [ColumnType] -> Cells
columnTypesToCells columns = res
  where
    columnOfStrings = map columnToFields columns 
    res = transpose columnOfStrings


addPercentColumns :: Sheet -> Sheet
addPercentColumns sheet = res
  where
    columns = columnTypes sheet
    mbNewColumns = map calcPercentColumn columns
    newColumns = catMaybes mbNewColumns
    allColumns = columns ++ newColumns
    res = if null newColumns
      then sheet
      else sheet {cells = (columnTypesToCells allColumns)}


-------------------------------------- Main ------------------------------------

handleText :: MulitLine -> Config -> MulitLine
handleText text conf = res
  where
    sheet = readSheet text
    codeMode = code conf
    percentMode = percent conf
    allQuoteMode = quote conf
    augmentedSheet = if percentMode
      then addPercentColumns sheet
      else sheet
    res = if codeMode
      then toCode augmentedSheet 
      else if allQuoteMode
        then toQuotedCsv augmentedSheet 
        else toCsv augmentedSheet 

