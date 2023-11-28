module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


-- Exercise 6
data Calendar = Calendar {events :: [Event]
                         , calProps :: [CalProp]}
    deriving (Eq, Ord)

data CalProp = ProdId {prodId :: String}
             | Version {version :: Float }
    deriving (Eq, Ord)

data Event = Event {eventProps :: [EventProp]}
    deriving (Eq, Ord)

data EventProp = DtStamp {dtStamp :: DateTime} 
               | Uid {uid :: String}
               | DtStart {dtStart :: DateTime}
               | DtEnd {dtEnd :: DateTime}
               | Description {description :: String}
               | Summary {summary :: String}
               | Location {location :: String}
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


--pack function parser a to parser b to parser c for 7