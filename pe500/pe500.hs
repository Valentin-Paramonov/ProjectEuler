import PE.Primes
import Data.Heap as Heap
import GHC.List as List

f k = 
    let 
        ps = List.take k primes
    in
        f' k 1 (Heap.fromList ps :: MinHeap Integer)

f' 0 n _ = n
f' k n divisors =
    let 
        (e, newHeap) = pop divisors
    in
        f' (k - 1) (n*e) (Heap.insert (e^2) newHeap) 

pop heap =
    let 
        [e] = Heap.take 1 heap
        newHeap = Heap.drop 1 heap
    in 
        (e, newHeap)

main = do
    print $ f 500500 `mod` 500500507

