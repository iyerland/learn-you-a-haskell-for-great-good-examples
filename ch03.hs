lucky :: Int -> String
lucky 7 = "LUCKY!"
lucky x = "UNLUCKY!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe 6 = "Six"
sayMe 7 = "Seven"
sayMe 8 = "Eight"
sayMe 9 = "Nine"
sayMe x = "Not between 1 & 9!"

charName :: Char -> String
charName 'g' = "Gayatri"
charName 'n' = "Narayan"
charName 'v' = "Vimalananda"
charName  x  = "Please enter either g, n or v to get a valid response!"

myfact :: Integer -> Integer
myfact 0 = 1
myfact n = n * myfact (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' []    = error "Can't call head on an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []          = "This list is empty"
tell (x:[])      = "This list has one element: " ++ show x
tell (x:y:[])    = "This list has two elements: " ++ show x ++ " and " ++ show y
tell all@(x:y:_) = "This list has " ++ show (length all) ++ " elements. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter ""         = "Empty string!"
firstLetter all@(x:xs) = "This first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
   | bmi <= 18.5 = "You are underweight."
   | bmi <= 25.0 = "You are normal."
   | bmi <= 30.0 = "You're fat!"
   | otherwise   = "You're a whale, congrats!"
   
bmiShout :: Double -> Double -> String
bmiShout weight height
   | bmi <= 18.5 = printSkinny
   | bmi <= 25.0 = printNormal
   | bmi <= 30.0 = printOverweight
   | otherwise   = printWhale
   where bmi    = weight / height ^ 2
         skinny = 18.5
         normal = 25.0
         fat    = 30.0
         printSkinny     = "You're Underweight!"
         printNormal     = "You're Normal!"
         printOverweight = "You're Overweight!"
         printWhale      = "You're Whale!"
   
max' :: (Ord a) => a -> a-> a
max' a b
   | a <= b    = b
   | otherwise = a
   
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
   | a == b       = EQ
   | a < b        = LT
   | otherwise    = GT
   
badGreeting :: String
badGreeting = "Oh! It's you!"

niceGreeting :: String
niceGreeting = "Hello! So nice to see you!"

greet :: String -> String
greet "Narayan"      = niceGreeting ++ " Juan"
greet "Gayatri"      = niceGreeting ++ " Fernando"
greet "Vimalananda"  = niceGreeting ++ " Vimalananda"
greet name           = badGreeting  ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
   where (f:_) = firstname
         (l:_) = lastname
         
calcbmi :: [(Double, Double)] -> [Double]
calcbmi xs = [bmi w h  | (w, h) <- xs]
             where bmi weight height = weight / height ^ 2
        
cylinder :: Double -> Double -> Double
cylinder radius height =
            let sideArea = 2 * pi * radius * height
                topArea  = pi * radius ^ 2
            in  sideArea + 2 * topArea
            
head'' :: [a] -> a
head'' xs = case xs of []     -> error "No head for empty lists!"
                       (x:_)  -> x
                       
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty!"
                                               [x] -> "a singleton list!"
                                               xs  -> "a longer list!"   