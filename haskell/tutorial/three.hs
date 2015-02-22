module Three where
import Data.List
import qualified Data.Set as Set

data1 = [234,234,234,34,3,543,5,465,56,7,68,678,56,7,45,34,53,5,34,52343534]

data2 = data1 ++ reverse data1 ++ data1 ++ reverse data1
data3 = data2 ++ reverse data2 ++ data2 ++ data1

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x: []) = [x]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where (smaller,larger) = partition (<= x) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x: []) = [x]
msort xs = merge smaller larger
  where n = div (length xs) 2
        smaller = msort $ take n xs
        larger = msort $ drop n xs
        merge l1 [] = l1
        merge [] l2 = l2
        merge (y:ys) (z:zs)
          | y <= z = [y] ++ merge ys (z:zs)
          | otherwise = [z] ++ merge (y:ys) zs

isprime :: Eq a => a -> [(a,Bool)] -> Bool
isprime key [] = False
isprime key xs
  | null fxs = True
  | otherwise = snd $ head fxs
  where fxs = filter (\ (k,v) -> k == key) xs

sieve :: Int -> [Int]
sieve lim = looper 1 [(1,False), (2,True)]
  where size = ceiling $ sqrt $ fromIntegral lim
        looper i xs 
          | i > lim = filter (\x -> isprime x xs) $ 2 : [3,5..lim]
          | i <= size = if isprime i xs
                        then looper (i+2) loopi
                        else looper (i+2) xs
          | otherwise = looper (i+2) xs
          where loopi = xs ++ map (\x -> (x,False)) [i*i, (i* (i+2))..lim]


primes :: [Int]
primes = 2 : filter (\x -> not (any (\y -> 0 == rem x y) $ tmp x)) [3,5..]
  where tmp i = takeWhile (\x -> x*x <= i) primes

fibo :: Int -> Integer
fibo = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = fibo (n-1) + fibo (n-2)

euler25 :: Integer -> Int
euler25 lim = length $ takeWhile (< lim) (map fibo [0..])

lim = 10^999

fiboLim :: Integer -> Int
fiboLim lim = looper 1 0 1
  where looper a b i = if a > lim then i else looper (a+b) a (succ i)



