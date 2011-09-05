import Data.List (nub, sort)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

phoneBook = 
  [("betty", "555-2939"),
   ("bonnie", "345-2345")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
