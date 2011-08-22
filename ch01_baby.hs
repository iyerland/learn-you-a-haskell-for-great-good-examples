doubleMe x = x + x

doubleUs x y = doubleMe (x + y)

doublesmallnumber x = if x > 100
                        then x              
                        else doubleMe x             
                        
mysecond xs = xs !! 1  

mysecondfromlast xs = xs !! (length xs - 2)

mylast xs = xs !! (length xs - 1)  

length' xs = sum [1 | _ <- xs]

removenonuppercase st = [c | c <- st, c `elem` ['A'..'Z']]

triples = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10] ]

rightTriangles = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2 ]
