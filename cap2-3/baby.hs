doubleUs x y = doubleMe x + doubleMe y
doubleMe x = x + x
doubleSmallNumber 
  x = if x > 100 
    then x
    else x*2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "Its a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

combine xs ys = [x ++ " " ++ y | x <- xs, y <- ys]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

i = (2 , 3)