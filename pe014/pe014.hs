import qualified Data.Map.Lazy as Map

collatz n ls = case (Map.lookup n ls) of
    (Just x) -> (x,ls)
    Nothing -> let c = collatz' n ls in
        (c, Map.insert n c ls)
collatz' n ls
    | even n = (fst . collatz (n `div` 2) $ ls) + 1
    | otherwise = (fst . collatz (3*n + 1) $ ls) + 1

main = print . fst . fst . foldl maxCollatz ((0,0), Map.singleton 1 1) $ [1..limit] where
    maxCollatz (mx@(m,lm),ls) n
        | ln > lm = ((n,ln),nls)
        | otherwise = (mx,nls)
        where (ln, nls) = collatz n ls
    limit = 10^6 - 1
