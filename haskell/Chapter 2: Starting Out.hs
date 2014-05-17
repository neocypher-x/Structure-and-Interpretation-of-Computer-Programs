doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                         then x
                         else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a me-, Conan O'Brien!"

lostNumbers = [4,8,15,16,23,42]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

not13or15or19 = [ x | x <- [10..20], x/= 13, x /= 15, x/= 19]

allCombinationsSum = [ x+y | x <- [1,2,3], y <- [10,100,1000]]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy", "grouchy", "scheming"]
combineNounsAdjectives = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
