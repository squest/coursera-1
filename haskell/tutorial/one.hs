import Data.List

square x = x * x

pitagoras lim = [ a+b+c |a <- [1..lim],
                 b <- [1..lim], c <- [1..lim], (a^2) + (b^2) == c^2]

soal1 = [x|x <- [1..1000], ((rem x 3) == 0) || ((rem x 5) == 0)] 

factorial n = product [1..n]

-- n! = n x (n-1)!
-- n = 0 => n! = 1

factorial' 0 = 1
factorial' n = n * (factorial' (n - 1))

soal2 = [x| x <- [(-10)..10], (2*x > (-5)) && (2*x < 30)]

sample1 = [234,34,534,54,6,456,56,76,786,78,56,4,56,34,54,6,456]

sum' [] = 0
sum' xs = (head xs) + (sum' $ tail xs)

numcol :: Int -> [Int]
numcol n
  | n < 10 = [n]
  | otherwise = (numcol $ div n 10) ++ [mod n 10]

gabung lim = foldl1 (++) $ map numcol [1..lim]

div' :: Int -> Int -> Bool
div' a b = (mod a b) == 0

add2 x = x + 2

prime' n
  | n == 1 = False
  | n == 2 = True
  | even n = False
  | otherwise = if (length primeHelper) == 0 then True else False
  where primeHelper = filter (\x -> div' n x) [3,5..(pred n)]




