data Date = Date Int Int Int

instance Eq Date where
    (Date y m d) == (Date y' m' d') = 
        y == y' && m == m' && d == d'

instance Ord Date where
    compare (Date y m d) (Date y' m' d') =
        let compareYears = compare y y'
            compareMonths = compare m m'
            compareDays = compare d d'
        in if compareYears == EQ then
            if compareMonths == EQ then
                compareDays
            else compareMonths
        else compareYears

daysToAdd :: Int -> Int -> Int
daysToAdd 0 _ = 31
daysToAdd 1 y = 
    if (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)
    then 29
    else 28
daysToAdd 2 _ = 31
daysToAdd 3 _ = 30
daysToAdd 4 _ = 31
daysToAdd 5 _ = 30
daysToAdd 6 _ = 31
daysToAdd 7 _ = 31
daysToAdd 8 _ = 30
daysToAdd 9 _ = 31
daysToAdd 10 _ = 30
daysToAdd 11 _ = 31

countDays :: Int -> Date -> Date -> Int
countDays dow startDate@(Date y m d) endDate
    | startDate > endDate = n
    | otherwise = n + (countDays newDow newDate endDate)
    where n = if dow == 6 then 1
              else 0
          newDate = 
            let newMonth = (m + 1) `mod` 12
                newYear = if newMonth == 0 then y + 1 else y
            in (Date newYear newMonth d)
          newDow = (dow + (daysToAdd m y)) `mod` 7

main = do
    print $ countDays 1 (Date 1901 0 1) (Date 2000 11 31)
