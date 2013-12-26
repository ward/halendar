-- The repeating mentioned here includes skipping over a month that doesn't
-- have the particular day we are following. Eg 31 January will have 31 March
-- next in the line.

-- TODO, we are using iterate, which doesn't have a strict version built in
-- Use http://stackoverflow.com/q/8909997/411495 ?
-- See also freenode/#haskell around 2013-12-23 15:30 UTC

module Time (
    repeatDaily
  , repeatMonthly
  , repeatYearly
  , getYear
  , getMonth
  , getDay
  , startOfMonth
  , endOfMonth) where


import Data.Time

repeatDaily :: UTCTime -> [UTCTime]
repeatDaily = iterate addDay

addDay :: UTCTime -> UTCTime
addDay t = UTCTime (addDays 1 $ utctDay t) (utctDayTime t)

repeatMonthly :: UTCTime -> [UTCTime]
repeatMonthly = iterate addMonth

addMonth :: UTCTime -> UTCTime
addMonth (UTCTime day difftime) = UTCTime nextOkMonth difftime
    where
        nextOkMonth :: Day
        nextOkMonth = head $ dropWhile (differentday day) $ zipWith addGregorianMonthsClip [1..] (repeat day)

-- Checks if the actual day number in a month is the same.
-- For example 30th january = 30th march = 30th april = ...
differentday :: Day -> Day -> Bool
differentday d1 d2 = differentthirdtuple (toGregorian d1) (toGregorian d2)
differentthirdtuple :: Eq c => (a, b, c) -> (a, b, c) -> Bool
differentthirdtuple (_, _, x3) (_, _, y3) = x3 /= y3

repeatYearly :: UTCTime -> [UTCTime]
repeatYearly = iterate addYear

addYear (UTCTime day difftime) = UTCTime nextOkYear difftime
    where
        nextOkYear :: Day
        nextOkYear = head $ dropWhile (differentday day) $ zipWith addGregorianYearsClip [1..] (repeat day)

getYear  :: UTCTime -> Integer
getYear  = (\(y,_,_) -> y) . toGregorian . utctDay
getMonth :: UTCTime -> Int
getMonth = (\(_,m,_) -> m) . toGregorian . utctDay
getDay   :: UTCTime -> Int
getDay   = (\(_,_,d) -> d) . toGregorian . utctDay

startOfMonth :: UTCTime -> UTCTime
startOfMonth (UTCTime d _) = UTCTime (toBegin $ toGregorian d) 0
    where
        toBegin :: (Integer, Int, Int) -> Day
        toBegin (y, m, _) = fromGregorian y m 1
endOfMonth :: UTCTime -> UTCTime
endOfMonth (UTCTime d _) = UTCTime (toEnd $ toGregorian d) 86401
    where
        toEnd :: (Integer, Int, Int) -> Day
        toEnd (y, m, _) = fromGregorian y m (gregorianMonthLength y m)
