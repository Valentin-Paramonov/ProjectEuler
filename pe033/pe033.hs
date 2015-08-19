import PE.Number
import Data.Ratio

fractions a b n = [[a,n] # [b,n], [a,n] # [n,b], [n,a] # [b,n], [n,a] # [n,b]] where
    (#) n d = toNumber n % toNumber d

main = print . product . flatMap cancelling $ pool where
    cancelling (a,b) = filter (== a % b) . flatMap (filter (<1) . fractions a b) $ [1..9]
    flatMap f = foldl1 (++) . map f
    pool = [(a,b) | a <- [1..9], b <- [1..9]]
