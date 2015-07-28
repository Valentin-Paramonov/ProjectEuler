
					-- Problem 2 --

fib 0  = []
fib 1  = [1]
fib 2  = [2,1]
fib n = (x1 + x2) : prev
	where prev = fib $ n - 1
	      (x1:x2:_) = prev

--sum([x | x <- (fib 100), x <= 4e6, even x])
