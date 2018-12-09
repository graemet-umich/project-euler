-- Counting Sundays

-- By the way, 1200 // 7 = 171.

module Problem019
    ( problem019
    ) where

-- Weekdays are numbered Mondays (1) to Sundays (7)
(monday, sunday) = (1, 7)

problem019 :: IO ()
problem019 =
  print .
  length .
  filter (\(day, (_, d, _)) -> day == sunday && d == 1) .
  dropWhile (\(_, (_, _, y)) -> y < 1901) $
  dayDates

dayDates = zip (cycle [monday .. sunday]) dates

dates = [ (m, d, y) | y <- [1900 .. 2000],
                      m <- [1 .. 12],
                      d <- [1 .. getMaxDay m y] ]

getMaxDay :: Int -> Int -> Int
getMaxDay m y
  | m ==  2 = daysInFeb y
  | m ==  4 = 30
  | m ==  6 = 30
  | m ==  9 = 30
  | m == 11 = 30
  | otherwise = 31

daysInFeb :: Int -> Int
daysInFeb y
  | rem y 400 == 0 = 29
  | rem y 100 == 0 = 28
  | rem y   4 == 0 = 29
  | otherwise      = 28
