-- The repeating mentioned here includes skipping over a month that doesn't
-- have the particular day we are following. Eg 31 January will have 31 March
-- next in the line when repeating monthly.

module Time (
    repeatDaily
  , repeatMonthly
  , repeatYearly
  , getYear
  , getMonth
  , getDay) where


import Data.Time

repeatDaily :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
repeatDaily = repeatRange addDay

repeatMonthly :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
repeatMonthly (s, e) = filter (sameDays (s, e)) $ repeatRange addMonthClip (s, e)

repeatYearly :: (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
repeatYearly (s, e) = filter (sameDays (s, e)) $ repeatRange addYearClip (s, e)

-- Check whether the day number (in a month) is the same, pairwise.
sameDays :: (UTCTime, UTCTime) -> (UTCTime, UTCTime) -> Bool
sameDays (start, end) (start', end') = sameDay start start' && sameDay end end'
sameDay :: UTCTime -> UTCTime -> Bool
sameDay (UTCTime d1 _) (UTCTime d2 _) = sameLast3 (toGregorian d1) (toGregorian d2)
    where
        sameLast3 :: Eq c => (a, b, c) -> (a, b, c) -> Bool
        sameLast3 (_, _, x3) (_, _, y3) = x3 == y3

-- Indiscriminately starts repeating a date, the mentioned filtering is done
-- through sameDays.
repeatRange :: (Integer -> UTCTime -> UTCTime) -> (UTCTime, UTCTime) -> [(UTCTime, UTCTime)]
repeatRange f range = zipWith fTuple [0..] $ repeat range
    where
        fTuple :: Integer -> (UTCTime, UTCTime) -> (UTCTime, UTCTime)
        fTuple i (start, end) = (f i start, f i end)

addDay :: Integer -> UTCTime -> UTCTime
addDay i (UTCTime day time) = UTCTime (addDays i day) time

-- The clipping indicates the same clipping as in Data.Time.Calendar
-- eg: 31 March + 1 month = 30 April
addMonthClip :: Integer -> UTCTime -> UTCTime
addMonthClip i (UTCTime day time) = UTCTime (addGregorianMonthsClip i day) time

addYearClip :: Integer -> UTCTime -> UTCTime
addYearClip i (UTCTime day time) = UTCTime (addGregorianYearsClip i day) time

--------------------------------------------------------------------------------

getYear  :: UTCTime -> Integer
getYear  = (\(y,_,_) -> y) . toGregorian . utctDay
getMonth :: UTCTime -> Int
getMonth = (\(_,m,_) -> m) . toGregorian . utctDay
getDay   :: UTCTime -> Int
getDay   = (\(_,_,d) -> d) . toGregorian . utctDay
