module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.List
import Data.Maybe

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = f <$> parseDate <*> symbol 'T' <*> parseTime <*> parseTimeUTC
    where f date _ time utc = DateTime date time utc

parseDate :: Parser Char Date -- todo refactor
parseDate = f <$> parseYear <*> parseMonth <*> parseDay
    where f year month day = Date year month day

parseYear :: Parser Char Year
parseYear = f <$> parseIntQuartet
    where f year = Year year

parseMonth :: Parser Char Month
parseMonth = f <$> parseIntPair
    where f month = Month month

parseDay :: Parser Char Day
parseDay = f <$> parseIntPair
    where f day = Day day

parseTime :: Parser Char Time
parseTime = f <$> parseHour <*> parseMinute <*> parseSecond
    where f hour minute second = Time hour minute second

parseHour :: Parser Char Hour
parseHour = f <$> parseIntPair
    where f hour = Hour hour

parseMinute :: Parser Char Minute
parseMinute = f <$> parseIntPair
    where f minute = Minute minute

parseSecond :: Parser Char Second
parseSecond = f <$> parseIntPair
    where f second = Second second

parseTimeUTC :: Parser Char Bool
parseTimeUTC = const True <$> symbol 'Z'
            <|> succeed False

parseIntPair :: Parser Char Int
parseIntPair = f <$> natural <*> natural
    where f p q = p * 10 + q

parseIntQuartet :: Parser Char Int
parseIntQuartet = f <$> parseIntPair <*> parseIntPair
    where f p q = p * 100 + q

-- Exercise 2
run :: Eq a => Parser a b -> [a] -> Maybe b
run parser input = 
    case result of
        Just result -> Just $ fst result
        _ -> Nothing      
    where
        output = parse parser input
        result = find (\(_, tail) -> null tail) output

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime date time utc)
    = printDate date ++ "T" ++ printTime time ++ printUTC utc

printDate :: Date -> String
printDate (Date (Year y) (Month m) (Day d))
    = y' ++ m' ++ d'
    where
        y' = padInteger y 4
        m' = padInteger m 2
        d' = padInteger d 2

printTime :: Time -> String
printTime (Time (Hour h) (Minute m) (Second s))
    = h' ++ m' ++ s'
    where
        h' = padInteger h 2
        m' = padInteger m 2
        s' = padInteger s 2

printUTC :: Bool -> String
printUTC utc | utc = "Z"
             | otherwise = ""

-- https://stackoverflow.com/questions/32311461/how-do-i-pad-string-representations-of-integers-in-haskell
padInteger :: Int -> Int -> String
padInteger number desiredLength = replicate (desiredLength - numberLength) '0' ++ show number
    where
        numberLength = length $ show number

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime date time utc) = checkDate date && checkTime time

checkDate :: Date -> Bool
checkDate (Date y m d) = checkYear y && checkMonth m && checkDay d
 
checkYear :: Year -> Bool
checkYear (Year y) = checkRange 1 4 $ length $ show y

checkMonth :: Month -> Bool
checkMonth (Month m) = checkRange 1 12 m

checkDay :: Year -> Month -> Day -> Bool
checkDay year@(Year y) (Month m) (Day d)
    = checkRange 1 28 d 
   || elem m month_30 && d == 30
   || elem m month_31 && d == 31
   || Year `mod` 4 == 0
    -- 30 
    -- 31
    where
        month_30 = [4, 6, 9, 11]
        month_31 = [1, 3, 5, 7, 8, 10, 12]

checkDay :: Year -> Month -> Day -> Bool
checkDay (Year y) (Month m) (Day d)
    = checkRange 1 28 d
    -- 30 
    -- 31
    where
        month_30 = [4, 6, 9, 11]
        month_31 = [1, 3, 5, 7, 8, 10, 12]

monthMaxRange :: Int -> Int
monthMaxRange m | elem m month_30 = 30
                | elem m month_31 = 31
                | otherwise = 28
    where
        month_30 = [4, 6, 9, 11]
        month_31 = [1, 3, 5, 7, 8, 10, 12]

checkFebruary29 :: Year -> Bool
checkFebruary29 (Year y) = 

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s))
    = checkRange 0 23 h
    && checkRange 0 59 m
    && checkRange 0 59 s

checkRange :: Int -> Int -> Int -> Bool
checkRange min max value = value >= min && value <= max