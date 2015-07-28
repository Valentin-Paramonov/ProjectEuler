   
					-- Problem 1 --

mults lim = [ x | x <- [1..(lim-1)], x `mod` 3 == 0  || x `mod` 5 == 0]
