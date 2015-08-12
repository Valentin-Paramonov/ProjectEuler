import Data.Set

main = print . size . fromList $ [a^b | a <- ns, b <- ns] where
    ns = [2..100]
