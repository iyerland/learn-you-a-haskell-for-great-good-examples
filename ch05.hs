divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUppercase :: Char -> Bool
isUppercase = (`elem` ['A'..'Z'])

apply2 :: (a -> a) -> a -> a
apply2 f x = f (f x)

zipw :: (a -> b -> c) -> [a] -> [b] -> [c]
zipw _ [] _   = []
zipw _ _ [] = []
zipw f (x:xs) (y:ys) = f x y : zipw f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
   | p x       = x : filter' p xs
   | otherwise = filter' p xs
   
filterQsort :: (Ord a) => [a] -> [a]
filterQsort [] = []
filterQsort (x:xs) =
   let smallerOrEqual = filter (<= x) xs
       larger = filter (> x) xs
   in filterQsort smallerOrEqual ++ [x] ++ filterQsort larger
   
-- find the largest number under 10000 thats divisible by 3829
largestDiv :: Integer
largestDiv = head (filter p [10000, 9999..])
   where p x = x `mod` 3829 == 0
   
-- find the sum of all odd squares that are smaller than 10000.
-- sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

-- For all starting numbers between 1-100, how many Collatz chain have
-- length > 15
-- Collatz Chain: 
--	1. Start with a Natural Number
--	2. If 1 then stop
--	3. If even divide by 2
--	4. If odd multiply by 3 + 1
--  4. Repeat the algorithm with resulting number

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
	| even n = n : chain (n `div` 2)
	| odd  n = n : chain (n * 3 + 1)
	
numLongChains :: Int
numLongChains = length (filter isLengthGreaterThanFifteen (map chain [1..100]))	
	where isLengthGreaterThanFifteen xs = length xs > 15

-- this uses anonymous function or lambda to flip its parameters (x,y) and then 
-- apply the function f.

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x


