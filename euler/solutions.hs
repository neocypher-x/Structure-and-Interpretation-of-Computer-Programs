e001 = sum[x | x <- [1..1000], x `mod` 3 == 0, x `mod` 5 == 0]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- x from 1 to 33 because fib x is last fibonacci less than 4 mil
-- by trial and error.
e002 = sum [fib x | x <- [1..33], fib x `mod` 2 == 0]
