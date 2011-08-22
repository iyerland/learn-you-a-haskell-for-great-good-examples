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

