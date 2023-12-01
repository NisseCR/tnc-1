module Calendar where

import ParseLib.Abstract
import Data.List
import qualified Data.Set as Set
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

-- Create sets of 3 tokens (start, content, end) for each element in the grammar.
scanCalendar :: Parser Char [Token]
scanCalendar =  concat <$> greedy (scanToken <* scanTokenBreak)
    where
        scanTokenBreak = token "\r\n" -- w/o spaces

-- Match token parser based on grammar prefix (e.g. DTSTAMP:).
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

-- Simply return TokenWrapper, since this token does not contain data.            
scanWrapper :: String -> Parser Char [Token]
scanWrapper prefix = const [TokenWrapper] <$> token prefix 

-- == DateTime tokens ==
scanDateTimeToken :: String -> Parser Char [Token]
scanDateTimeToken prefix = assignDateTimeToken prefix <$> (token prefix *> parseDateTime)

assignDateTimeToken :: String -> DateTime -> [Token]
assignDateTimeToken "DTSTAMP:" result = [TokenDtStamp, TokenDateTime result, TokenCrlf]
assignDateTimeToken "DTSTART:" result = [TokenDtStart, TokenDateTime result, TokenCrlf]
assignDateTimeToken "DTEND:" result = [TokenDtEnd, TokenDateTime result, TokenCrlf]

-- == Text tokens ==
scanTextToken :: String -> Parser Char [Token]
scanTextToken prefix = assignTextToken prefix <$> (token prefix *> parseText)

assignTextToken :: String -> String -> [Token]
assignTextToken "UID:" result = [TokenUid, TokenText result, TokenCrlf]
assignTextToken "DESCRIPTION:" result = [TokenDescription, TokenText result, TokenCrlf]
assignTextToken "SUMMARY:" result = [TokenSummary, TokenText result, TokenCrlf]
assignTextToken "LOCATION:" result = [TokenLocation, TokenText result, TokenCrlf]
assignTextToken "PRODID:" result = [TokenProdId, TokenText result, TokenCrlf]
assignTextToken "VERSION:" result = [TokenVersion, TokenText result, TokenCrlf]

-- == Parsing of multiline text ==
-- Parse text including newlines that are *not* a delimiter for a token. This function parses until a crlf w/o space.
parseText :: Parser Char String
parseText = f <$> parseLines <*> parseLine
    where
        f ls l = tail $ intercalate " " ls ++ " " ++ l

-- Parse lines ending with crlf w/ space.
parseLines :: Parser Char [String]
parseLines = greedy $ parseLine <* parseBreak
    where
        parseBreak = token "\r\n "

-- Parse single line *until* crlf w/o space.
parseLine :: Parser Char String
parseLine = greedy $ satisfy (\c -> c /= '\r')


-- == Calendar parsing ==
parseCalendar :: Parser Token Calendar
parseCalendar = pack (symbol TokenWrapper) parseCalendarContent (symbol TokenWrapper)
    where
        parseCalendarContent = Calendar <$> many parseCalendarProp <*> many parseEvent >>= checkCalendar
    
parseCalendarProp :: Parser Token CalProp
parseCalendarProp = ProdId <$> pack (symbol TokenProdId) (parseTextToken) (symbol TokenCrlf)
                 <|> Version <$> pack (symbol TokenVersion) (parseTextToken) (symbol TokenCrlf)

-- == Check calendar ==
checkCalendar :: Calendar -> Parser Token Calendar
checkCalendar calendar = if (abideOccurence && abideRequired) then succeed calendar else empty
    where
        props = map bindCalendarProp $ calProps calendar
        abideOccurence = checkOccurence props
        abideRequired = checkRequired props [TokenProdId, TokenVersion]
        
-- Bind properties back to tokens, to allow creation of sets by ignoring contained data.
bindCalendarProp :: CalProp -> Token
bindCalendarProp (ProdId _) = TokenProdId
bindCalendarProp (Version _) = TokenVersion

-- == Event parsing ==
parseEvent :: Parser Token Event
parseEvent = pack (symbol TokenWrapper) parseEventContent (symbol TokenWrapper) 
    where
        parseEventContent = Event <$> many parseEventProp >>= checkEvent

parseEventProp :: Parser Token EventProp
parseEventProp = DtStamp <$> pack (symbol TokenDtStamp) (parseDateTimeToken) (symbol TokenCrlf)
              <|> Uid <$> pack (symbol TokenUid) (parseTextToken) (symbol TokenCrlf)
              <|> DtStart <$> pack (symbol TokenDtStart) (parseDateTimeToken) (symbol TokenCrlf)
              <|> DtEnd <$> pack (symbol TokenDtEnd) (parseDateTimeToken) (symbol TokenCrlf)
              <|> Description <$> pack (symbol TokenDescription) (parseTextToken) (symbol TokenCrlf)
              <|> Summary <$> pack (symbol TokenSummary) (parseTextToken) (symbol TokenCrlf)
              <|> Location <$> pack (symbol TokenLocation) (parseTextToken) (symbol TokenCrlf)

-- == Extract data from token ==
-- https://ics.uu.nl/docs/vakken/b3tc/downloads-2018/monadExamples.hs
parseTextToken :: Parser Token String
parseTextToken = anySymbol >>= f
    where
        f (TokenText t) = succeed t
        f _ = empty


parseDateTimeToken :: Parser Token DateTime
parseDateTimeToken = anySymbol >>= f
    where
        f (TokenDateTime dt) = succeed dt
        f _ = empty

-- == Check event ==
checkEvent :: Event -> Parser Token Event
checkEvent event = if (abideOccurence && abideRequired) then succeed event else empty
    where
        props = map bindEventProp $ eventProps event
        abideOccurence = checkOccurence props
        abideRequired = checkRequired props [TokenDtStamp, TokenUid, TokenDtStart, TokenDtEnd]
        
-- Bind properties back to tokens, to allow creation of sets by ignoring contained data.
bindEventProp :: EventProp -> Token
bindEventProp (DtStamp _) = TokenDtStamp
bindEventProp (Uid _) = TokenUid
bindEventProp (DtStart _) = TokenDtStart
bindEventProp (DtEnd _) = TokenDtEnd
bindEventProp (Description _) = TokenDescription
bindEventProp (Summary _) = TokenSummary
bindEventProp (Location _) = TokenLocation

-- == Global validate functions ==
-- Check whether property does not occur more than once.
checkOccurence :: [Token] -> Bool
checkOccurence tks = length tks == (length $ Set.fromList tks)

-- Check whether required properties are present.
checkRequired :: [Token] -> [Token] -> Bool
checkRequired tks required = intersect required tks == required

-- == Main function ==
recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

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


-- == DEBUG ==
example :: String
example = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nBEGIN:VEVENT\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nSUMMARY:Bastille Day Party\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

debug :: Maybe Calendar
debug = recognizeCalendar $ printCalendar calen
    where
        (Just calen) = recognizeCalendar example

cal1 :: Calendar
cal1 = cal
    where
        (Just cal) = recognizeCalendar example

ev1 :: Event
ev1 = events cal1 !! 0


        