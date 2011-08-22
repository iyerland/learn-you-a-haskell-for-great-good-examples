maximum' :: (Ord a) => [a] -> a
maximum' []     = error "Maximum of empty list?!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x 
         | n <= 0    = []
         | otherwise = x : replicate' (n-1) x
         
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
   | n <= 0 = []
take' _ []  = []
take' n (x:xs) = x : take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
   | a == x = True
   | otherwise = a `elem'` xs
   
   
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
   let smallerOrEqual = [a | a <- xs, a <= x]
       larger = [a | a <- xs, a > x]
   in qsort smallerOrEqual ++ [x] ++ qsort larger
   

-- Currying example
-- f x y = (f x) y
--       = (g y)
--       = z
-- mult 2 6 = (mult 2) 6
--          = (double 6)
--          = 12
-- 3 `elem` [1,2,3]
-- g   = (3 `elem`) returns a function g which tests for presence of 3
--     = (g [1,2,3])            
--     = tests for presence of 3 in a list of [1,2,3]
--     = returns True | False
-- max 4 5
-- f(x,y) = max 4 5
-- (f x) y = (max 4) 5
-- g y = g 5
-- z   = 5
-- Mathematical Model
-- replicate 2 "a"
-- let n = 2
-- let x = "a"
-- replicate n x = x:replicate n-1
-- reverse [1,2,3]
-- let xs = [1,2,3]
-- reverse (x:xs) = reverse(xs) ++ [x]