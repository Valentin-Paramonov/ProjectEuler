import PE.Number
import Data.Set (fromList, toList)
import Data.List (permutations)

panProduct xs ss
    | mc * mp == p = p
    | otherwise = 0
    where
        mc:mp:[p] = map toNumber . slice ss $ xs

slice (f,s) xs = [a,b,c] where
    (a,rs) = splitAt f xs
    (b,c) = splitAt s rs

main = print . sum . toList . fromList $ products where
    products = foldl1 (++) . map panProducts $ panDigits
    panProducts xs = filter (/= 0) . map (panProduct xs) $ slices
    slices = [(1,4),(2,3)]
    panDigits = permutations [1..9]
