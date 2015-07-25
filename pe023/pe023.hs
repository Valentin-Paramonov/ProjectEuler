import qualified Data.Set as Set
import PE.Primes

main = do
    let limit = 28123
        firstAbundant = 12
        isAbundant n = pds n > n
        abundants = Set.fromList $ filter isAbundant [firstAbundant..limit]
        doesNotSum n = doesNotSum' (fst $ Set.split (n - firstAbundant + 1) abundants) n
        doesNotSum' as n
            | as == Set.empty = True
            | Set.member (n - a) as = False
            | otherwise = doesNotSum' (Set.deleteMin as) n
            where a = Set.findMin as
        nonSummable = filter doesNotSum [1..limit]
    print $ sum nonSummable
