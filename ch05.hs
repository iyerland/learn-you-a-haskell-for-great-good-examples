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
