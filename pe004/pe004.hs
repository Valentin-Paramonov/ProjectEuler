
					-- Problem 4 --

isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
	| x == (last xs) = isPalindrome $ init xs
	| otherwise      = False

----largestPalindrome :: (Integral a) => a -> a
largestPalindrome = lP x x []
	where x = truncate $ 10**3 - 1

lP :: (Integral a) => a -> a -> [(a,a,a)] -> [(a,a,a)]
lP 100 100 droms = droms
lP a b droms
	| isPalindrome $ show prod = g droms ++ [(prod,a,b)]
	| otherwise = g droms
--		if (a-1) == b then lP (a-1) b
--		else lP a (b-1)
	where prod = a*b
	      g =
			if b == 100 then lP (a-1) (a-1)
			else lP a (b-1)

--maximum [a | (a,b,c) <- largestPalindrome]
