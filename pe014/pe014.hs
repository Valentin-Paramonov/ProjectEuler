-- 23.296s
module Main where
--import Data.List
--import qualified Data.HashMap.Lazy as Map

--type IntMap = Map.HashMap Integer Integer
--main :: IO ()
main = do
    print $ longestCollatzUnder 1000000
    -- print $ maximumBy (\a b -> compare (snd a) (snd b)) $ [(n, lengthCollatz n) | n <- [1..999999]]
   
-- mapLengths resultList _ [] = resultList
-- mapLengths resultList tab (x:xs) =
--     mapLengths (resultList ++ [res]) newTab xs
--     where (res, newTab) = lengthCollatz x tab
--    
-- lengthCollatz :: Integer -> IntMap -> ((Integer, Integer), IntMap)
-- lengthCollatz 1 tab = ((1,1), Map.insert 1 1 tab)
-- lengthCollatz n tab = lengthCollatz' n n 1 tab
-- 
-- lengthCollatz' :: Integer -> Integer -> Integer -> IntMap -> ((Integer, Integer), IntMap)
-- lengthCollatz' n 1 l tab = ((n, l), Map.insert n l tab)
-- lengthCollatz' n c l tab =
--     case val of
--         Nothing -> ((c, nl), Map.insert c nl map2)
--         Just x -> ((c, x), tab)
--     where val = Map.lookup n tab
--           ((_,len), map2) = lengthCollatz' nxt nxt 1 tab
--           nl = len + 1
--           nxt = if even c 
--                    then div c 2 
--                    else 3 * c + 1

longestCollatzUnder n =
    longestCollatzUnder' n 1 1 1 (0,0)

longestCollatzUnder' n cur 1 l mx
    | l > (snd mx) = longestCollatzUnder' n ncur ncur 1 (cur,l)
    | otherwise = longestCollatzUnder' n ncur ncur 1 mx
    where ncur = cur + 1
longestCollatzUnder' n cur clz l mx
    | n == cur = mx
    | even clz = longestCollatzUnder' n cur (div clz 2) nl mx
    | otherwise = longestCollatzUnder' n cur (3 * clz + 1) nl mx
    where nl = l + 1

-- collatz :: (Integral a) => a -> [a]
-- collatz 1 = [1]
-- collatz x 
--     | even x = x : (collatz $ div x 2)
--     | otherwise = x : (collatz $ 3 * x + 1)
-- 
-- lengthCollatz :: (Integral a) => a -> a
-- lengthCollatz x = lengthCollatz' x 0
-- 
-- lengthCollatz' 1 l = l + 1
-- lengthCollatz' 2 l = l + 2
-- lengthCollatz' 4 l = l + 3
-- lengthCollatz' x l 
--     | even x = 
--             lengthCollatz' (div x 2) (l + 1)
--     | otherwise = lengthCollatz' (3 * x + 1) (l + 1)
-- -- foldl (\acc x -> if (snd acc) > (snd x) then acc else x) (0,0) [(n, length $ collatz n) | n <- [1..999999]]
