lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] =x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted = quicksort [a | a <- xs, a> x]
	in smallerSorted ++ [x] ++ biggerSorted

eval_helper :: Double => Double => Double => Integer => Double
eval_helper x y z p = (y/2)*sum[a**p | a<-[0,0.5..x]] + ((y*z) + 100) * (x + z)**p

eval :: Double => Double => Double => (Double,Double,Double,Double)
eval x y z = (
                (y/2)*x*2 + (y*z)+100,
                eval_helper x y z 1,
                eval_helper x y z 2,
                eval_helper x y z 3,
                )


eval_long :: Double => Double => Double => (Double,Double,Double,Double)
eval_long x y z = (
                (y/2)*x*2 + (y*z)+100,
                (y/2)*sum[a| a<-[0,0.5..x]] + ((y*z)+100)*(x+z),
                (y/2)*sum[a**2| a<-[0,0.5..x]]+ ((y*z)+100)*((x+z)**2),
                (y/2)*sum[a**3| a<-[0,0.5..x]]+((y*z)+100)*((x+z)**3)
                )

