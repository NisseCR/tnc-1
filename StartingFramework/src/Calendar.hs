module Calendar where

import ParseLib.Abstract
import Data.List
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
data Calendar = Calendar {calProps :: [CalProp]
                          , events :: [Event]}
    deriving (Eq, Ord, Show)

data CalProp = ProdId {prodId :: String}
             | Version {version :: String }
    deriving (Eq, Ord, Show)

data Event = Event {eventProps :: [EventProp]}
    deriving (Eq, Ord, Show)

data EventProp = DtStamp DateTime
               | Uid String
               | DtStart DateTime
               | DtEnd DateTime
               | Description String
               | Summary String
               | Location String
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = TokenText String
           | TokenDateTime DateTime
           | TokenWrapper
           | TokenDtStamp
           | TokenUid
           | TokenDtStart
           | TokenDtEnd
           | TokenDescription
           | TokenSummary
           | TokenLocation
           | TokenProdId
           | TokenVersion
           | TokenCrlf
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar =  concat <$> greedy (scanToken <* scanTokenBreak)

scanTokenBreak :: Parser Char [Token]
scanTokenBreak = const [TokenCrlf] <$> token "\r\n"

scanToken :: Parser Char [Token]
scanToken = scanWrapper "BEGIN:VCALENDAR"
         <|> scanWrapper "END:VCALENDAR"
         <|> scanWrapper "BEGIN:VEVENT"
         <|> scanWrapper "END:VEVENT"
         <|> scanDateTimeToken "DTSTAMP:"
         <|> scanDateTimeToken "DTSTART:"
         <|> scanDateTimeToken "DTEND:"
         <|> scanTextToken "UID:"
         <|> scanTextToken "DESCRIPTION:"
         <|> scanTextToken "SUMMARY:"
         <|> scanTextToken "LOCATION:"
         <|> scanTextToken "VERSION:"
         <|> scanTextToken "PRODID:"
            
scanWrapper :: String -> Parser Char [Token]
scanWrapper prefix = const [TokenWrapper] <$> token prefix 

-- Datetime tokens
scanDateTimeToken :: String -> Parser Char [Token]
scanDateTimeToken prefix = f <$> token prefix <*> parseDateTime
    where
        f _ datetime = assignDateTimeToken prefix datetime

assignDateTimeToken :: String -> DateTime -> [Token]
assignDateTimeToken "DTSTAMP:" result = [TokenDtStamp, TokenDateTime result, TokenCrlf]
assignDateTimeToken "DTSTART:" result = [TokenDtStart, TokenDateTime result, TokenCrlf]
assignDateTimeToken "DTEND:" result = [TokenDtEnd, TokenDateTime result, TokenCrlf]

-- Text tokens
scanTextToken :: String -> Parser Char [Token]
scanTextToken prefix = f <$> token prefix <*> parseText
    where
        f _ text = assignTextToken prefix text

assignTextToken :: String -> String -> [Token]
assignTextToken "UID:" result = [TokenUid, TokenText result, TokenCrlf]
assignTextToken "DESCRIPTION:" result = [TokenDescription, TokenText result, TokenCrlf]
assignTextToken "SUMMARY:" result = [TokenSummary, TokenText result, TokenCrlf]
assignTextToken "LOCATION:" result = [TokenLocation, TokenText result, TokenCrlf]
assignTextToken "PRODID:" result = [TokenProdId, TokenText result, TokenCrlf]
assignTextToken "VERSION:" result = [TokenVersion, TokenText result, TokenCrlf]

parseText :: Parser Char String
parseText = f <$> parseLines <*> parseLine
    where
        f ls l = tail $ intercalate " " ls ++ " " ++ l

parseLines :: Parser Char [String]
parseLines = greedy $ parseLine <* parseBreak

parseLine :: Parser Char String
parseLine = greedy $ satisfy (\c -> c /= '\r')

parseBreak :: Parser Char String
parseBreak = token "\r\n "

parseCalendar :: Parser Token Calendar
--parseCalendar = f <$> (symbol TokenWrapper *> many parseCalendarProps) <*> many parseEvents <* symbol TokenWrapper 
parseCalendar = pack (symbol TokenWrapper) parseCalendarContent (symbol TokenWrapper) 

parseCalendarContent ::  Parser Token Calendar
parseCalendarContent = f <$> many parseCalendarProp <*> many parseEvent
    where
        f props events = Calendar props events
    
parseCalendarProp :: Parser Token CalProp
parseCalendarProp = ProdId <$> pack (symbol TokenProdId) (ttext) (symbol TokenCrlf)
                 <|> Version <$> pack (symbol TokenVersion) (ttext) (symbol TokenCrlf)

parseEvent :: Parser Token Event
parseEvent = pack (symbol TokenWrapper) parseEventContent (symbol TokenWrapper) 

parseEventContent ::  Parser Token Event
parseEventContent = Event <$> many parseEventProp

parseEventProp :: Parser Token EventProp
parseEventProp = DtStamp <$> pack (symbol TokenDtStamp) (tdatetime) (symbol TokenCrlf)
              <|> Uid <$> pack (symbol TokenUid) (ttext) (symbol TokenCrlf)
              <|> DtStart <$> pack (symbol TokenDtStart) (tdatetime) (symbol TokenCrlf)
              <|> DtEnd <$> pack (symbol TokenDtEnd) (tdatetime) (symbol TokenCrlf)
              <|> Description <$> pack (symbol TokenDescription) (ttext) (symbol TokenCrlf)
              <|> Summary <$> pack (symbol TokenSummary) (ttext) (symbol TokenCrlf)
              <|> Location <$> pack (symbol TokenLocation) (ttext) (symbol TokenCrlf)

ttext :: Parser Token String -- TODO dafuq is this even
ttext = anySymbol >>= isTText
  where
    isTText :: Token -> Parser Token String
    isTText (TokenText t) = succeed t
    isTText _ = empty


tdatetime :: Parser Token DateTime -- TODO dafuq is this even 2
tdatetime = anySymbol >>= isTDateTime
  where
    isTDateTime :: Token -> Parser Token DateTime
    isTDateTime (TokenDateTime dt) = succeed dt
    isTDateTime _ = empty

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

example :: String
example = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

exomple :: String
exomple = "BEGIN:VCALENDAR\r\nVERSION: 2.0\r\nPRODID: -//hacksw/handcal//NONSGML v1.0//E\r\nN\r\nBEGIN:VEVENT\r\nUID: 19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY: Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

debug :: String
debug = printCalendar cal
    where
        (Just cal) = recognizeCalendar example

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar props events) = "BEGIN:VCALENDAR\r\n"
                                        ++ printCalProps 
                                        ++ printEvents
                                        ++ "END:VCALENDAR\r\n"
    where 
        printCalProps = concat $ map printCalProp props
        printEvents = concat $ map printEvent events

printCalProp :: CalProp -> String
printCalProp (ProdId c) = (printText $ "PRODID:" ++ c) ++ "\r\n"
printCalProp (Version c) = (printText $ "VERSION:" ++ c) ++ "\r\n"

printEvent :: Event -> String
printEvent (Event props) = "BEGIN:VEVENT\r\n" 
                            ++ (concat $ map printEventProp props)
                            ++ "END:VEVENT\r\n"

printEventProp :: EventProp -> String
printEventProp (DtStamp c) = "DTSTAMP:" ++ printDateTime c ++ "\r\n"
printEventProp (Uid c) = (printText $ "UID:" ++ c) ++ "\r\n" 
printEventProp (DtStart c) = "DTSTART:" ++ printDateTime c ++ "\r\n"
printEventProp (DtEnd c) = "DTEND:" ++ printDateTime c ++ "\r\n"
printEventProp (Description c) = (printText $ "DESCRIPTION:" ++ c) ++ "\r\n"
printEventProp (Summary c) = (printText $ "SUMMARY:" ++ c) ++ "\r\n"
printEventProp (Location c) = (printText $ "LOCATION:" ++ c) ++ "\r\n"

-- Ensure line length does not exceed 42 characters.
printText :: String -> String
printText line = printText' line [] 0

printText' :: String -> String -> Int -> String
printText' [] buffer _ = buffer
printText' text buffer 42 = buffer ++ "\r\n" ++ printText' text [] 0
printText' (x:xs) buffer bufferLength = printText' xs (buffer ++ [x]) (bufferLength + 1)


--pack function parser a to parser b to parser c for 7