main = do
    print . foldl1 max . filter isPalindrome $ threeDigitNumberProducts
    where
        isPalindrome = isP . show
        isP [] = True
        isP [x] = True
        isP (x:xs)
            | x == (last xs) = isP $ init xs
            | otherwise      = False
        threeDigitNumberProducts = [ i * j | i <- tripleDigitNumbers, j <- tripleDigitNumbers]
        tripleDigitNumbers = [100..999]
