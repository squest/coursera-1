module Two where

import Data.List
import Data.Char

csortBy f = sortBy (\x y -> compare (f x) (f y))
cgroupBy f ls = groupBy (\x y -> (f x) == (f y)) $ csortBy f ls

positions e ls = map snd $ filter (\x -> (fst x) == e) tempo
  where tempo = zip ls [0..]

let2int :: Char -> Int
let2int a = ord a - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


let2int' :: Char -> Int
let2int' a = ord a - ord 'A'

int2let' :: Int -> Char
int2let' n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let (mod (let2int c + n) 26)
  | isUpper c = int2let' (mod (let2int' c + n) 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

prod ls = helper ls 1
  where helper [] res = res
        helper (x:xs) res = helper xs (x*res)

deleteAll e ls = helper ls []
  where helper [] res = res
        helper (x:xs) res
          | x == e = helper xs res
          | otherwise = helper xs (res ++ [x])

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

ssort [] = []
ssort ls = helper ls []
  where helper [] res = res
        helper (x:xs) res = helper xs $ smaller ++ [x] ++ larger
          where smaller = takeWhile (< x) res
                larger = dropWhile (< x) res

isort [] = []
isort (x:xs) = smaller ++ [x] ++ larger
  where sorted = isort xs
        smaller = takeWhile (< x) sorted
        larger = dropWhile (< x) sorted

pitas = [x | a <- [3..29], b <- [5..38], let x = 2*a + 3*b]

sieve :: [Int]
sieve = 2 : helper [3,5..]
  where helper (x:xs) = x : filter (\e -> mod e x /= 0) (helper xs)

div' a b = rem a b == 0

prime' :: Int -> Bool
prime' n = not $ any (\x -> div' n x) (takeWhile (<= nsqrt) sieve)
  where nsqrt = round $ sqrt $ fromIntegral n

primes = 2 : filter prime' [3,5..]

sumaPrima lim = helper primes 0
  where helper (x:xs) res
          | x > lim = res
          | otherwise = helper xs (x+res)

sumPrimes lim = helper sieve 0
  where helper (x:xs) res
          | x > lim = res
          | otherwise = helper xs (x+res)


