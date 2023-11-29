module Calendar where

import ParseLib.Abstract
import Data.List
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
data Calendar = Calendar {events :: [Event]
                         , calProps :: [CalProp]}
    deriving (Eq, Ord)

data CalProp = ProdId {prodId :: String}
             | Version {version :: String }
    deriving (Eq, Ord)

data Event = Event {eventProps :: [EventProp]}
    deriving (Eq, Ord)

data EventProp = DtStamp DateTime
               | Uid String
               | DtStart DateTime
               | DtEnd DateTime
               | Description String
               | Summary String
               | Location String
    deriving (Eq, Ord)

-- Exercise 7
data Token = TokenEmpty
           | TCalProdId String
           | TCalVersion String
           | TEveDtStamp DateTime
           | TEveUid String
           | TEveDtStart DateTime
           | TEveDtEnd DateTime
           | TEveDescription String
           | TEveSummary String
           | TEveLocation String
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy $ scanToken <* token "\r\n"

scanToken :: Parser Char Token
scanToken = scanEmptyToken "BEGIN:VCALENDAR"
         <|> scanEmptyToken "END:VCALENDAR"
         <|> scanEmptyToken "BEGIN:VEVENT"
         <|> scanEmptyToken "END:VEVENT"
         <|> scanDateTimeToken "DTSTAMP:"
         <|> scanDateTimeToken "DTSTART:"
         <|> scanDateTimeToken "DTEND:"
         <|> scanDateTimeToken "DTSTAMP:"
         <|> scanDateTimeToken "DTSTART:"
         <|> scanDateTimeToken "DTEND:"
         <|> scanTextToken "UID:"
         <|> scanTextToken "DESCRIPTION:"
         <|> scanTextToken "SUMMARY:"
         <|> scanTextToken "LOCATION:"
         <|> scanTextToken "VERSION:"
         <|> scanTextToken "PRODID:"
            
scanEmptyToken :: String -> Parser Char Token
scanEmptyToken prefix = f <$> token prefix 
    where
        f _ = TokenEmpty

-- Datetime tokens
scanDateTimeToken :: String -> Parser Char Token
scanDateTimeToken prefix = f <$> token prefix <*> parseDateTime
    where
        f _ datetime = assignDateTimeToken prefix datetime

assignDateTimeToken :: String -> DateTime -> Token
assignDateTimeToken "DTSTAMP:" result = TEveDtStamp result
assignDateTimeToken "DTSTART:" result = TEveDtStart result
assignDateTimeToken "DTEND:" result = TEveDtEnd result

-- Text tokens
scanTextToken :: String -> Parser Char Token
scanTextToken prefix = f <$> token prefix <*> parseText
    where
        f _ text = assignTextToken prefix text

parseText :: Parser Char String
parseText = f <$> parseLines <*> parseLine
    where
        f ls l = intercalate " " ls ++ " " ++ l

parseLines :: Parser Char [String]
parseLines = greedy $ parseLine <* parseBreak

parseLine :: Parser Char String
parseLine = greedy $ satisfy (\c -> c /= '\r')

parseBreak :: Parser Char String
parseBreak = token "\r\n "

assignTextToken :: String -> String -> Token
assignTextToken "UID:" result = TEveUid result
assignTextToken "DESCRIPTION:" result = TEveDescription result
assignTextToken "SUMMARY:" result = TEveSummary result
assignTextToken "LOCATION:" result = TEveLocation result
assignTextToken "PRODID:" result = TCalProdId result
assignTextToken "VERSION:" result = TCalVersion result

parseCalendar :: Parser Token Calendar
parseCalendar = f <$> 
    where
        f props events = Calendar props events

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined


--pack function parser a to parser b to parser c for 7