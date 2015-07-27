import Data.List

main = do
    let x -: f = f x
        permutation = permutations [0..9] -: sort !! 999999
        toInt p = p -: map (show) -: concat -: read :: Int
    print $ toInt permutation
