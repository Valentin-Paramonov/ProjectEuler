import Data.Char

main = do
    print . sum . map digitToInt . show $ foldr1 (*) [2..100]
