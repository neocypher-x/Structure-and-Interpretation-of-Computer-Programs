--Number operations
mySucc = succ 7
myMin = min 3.99 3.999
myMax = max 1.00000000000001 1.0 -- if you add a few more zeros, 1.0 is larger

-- Basic functions
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y -- doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x > 100
                         then x
                         else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
conanO'Brien = "It's a me-, Conan O'Brien!"

-- Intro to lists
lostNumbers = [4,8,15,16,23,42]
combineLists = [1,2,3,4] ++ [5,6,7,8]
appendBack = [1,2,3,4,5] ++ [6] -- running time proportional to number of elements of first list.
consOperator = 'A':" SMALL CAT" -- appends to front of second list, constant time
consOperator2 = 5:[1,2,3,4,5]
--indexing into lists
_6element = "0123456" !! 6

slice = take 10 (cycle [1,2,3]) -- infinite list of 1,2,3
rep = take 10 (repeat 5) -- infinite list of 5's

-- list comprehensions
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
not13or15or19 = [ x | x <- [10..20], x/= 13, x /= 15, x/= 19]
allCombinationsSum = [ x+y | x <- [1,2,3], y <- [10,100,1000]]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy", "grouchy", "scheming"]
combineNounsAdjectives = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

likeLispCar  = head [5,4,3,2,1]
likeLispCdr = tail [5,4,3,2,1]

length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
nowLowerCase = removeNonUppercase "IdontLIKEFROGS"

-- Nested list comprehension
nestedList = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
oddFilter = [ [ x | x <- xs, even x] | xs <- nestedList]

-- Tuples
-- lists store an arbitrary number of elements of the same type.
-- tuples lift this restriction to allow you to store heterogeneous types

first = fst (8, 11) -- only works on pairs, not n-tuples
second = snd ("Wow", False)

myZip = zip [1 .. 5] ["one","two","three","four","five"] -- zip operates on two lists, returns a list of tuples
workingZip = zip [1..] ["apple", "banana","cherry"] -- works for differing length lists and lists of different types

-- Which right triangle has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?
possibleSolns = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
-- This implementation results in duplicate combinations
myFilteredSolns = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]
-- The below implementation eliminates duplicate combinations
filteredSolns = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
finalAnswer = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
