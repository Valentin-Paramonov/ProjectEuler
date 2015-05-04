module PE.Tree (
    Tree(Leaf,Node),
    toList
) where

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

toList :: Tree a -> [a]
toList t = toList' t []

toList' t ls = case t of
    (Leaf a) -> a:ls
    (Node ln rn) -> toList' ln ls ++ toList' rn ls

