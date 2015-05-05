import Data.Char
import System.IO

score name = sum $ map (\c -> ord c - 64) name

main = do
    file <- readFile "./names.txt"
    print . snd . foldl (\(n,sc) name -> (n + 1,score name * n + sc)) (1,0) $ lines file

