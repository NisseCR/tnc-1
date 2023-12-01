module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes


-- Exercise 9
-- 9.1
countEvents :: Calendar -> Int
countEvents calendar = length $ events calendar

-- 9.2 (partially implemented, comparison missing)
findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ events) = filter (eventMatch dt) events

eventMatch :: DateTime -> Event -> Bool
eventMatch dt (Event props) = dateMatch dt start end
    where
        start = findStart props
        end = findEnd props

dateMatch :: DateTime -> DateTime -> DateTime -> Bool
dateMatch (DateTime d t _) (DateTime d1 t1 _) (DateTime d2 t2 _)
    = undefined -- dt >= dt1 && dt < dt2

findStart :: [EventProp] -> DateTime
findStart [] = error "not possible"
findStart (x:xs) = case x of
    (DtStart dt) -> dt
    _ -> findStart(xs)

findEnd :: [EventProp] -> DateTime
findEnd [] = error "not possible"
findEnd (x:xs) = case x of
    (DtEnd dt) -> dt
    _ -> findEnd(xs)

-- 9.3
checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

-- 9.4
timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

