module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
data Calendar = Calendar
    deriving (Eq, Ord, Show)

data Event = Event {eventProps :: [EventProp]}
    deriving (Eq, Ord)

data EventProp = DtStamp | Summary
    deriving (Eq, Ord)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
