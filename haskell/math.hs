module Math where

import Data.List
import qualified Data.Set as Set
import Data.Ord

primeHelper :: Int -> Int -> Bool
primeHelper p i
  | (i*i) > p = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2)

-- it returns true if p is prime and false otherwise
prime' :: Int -> Bool
prime' p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = primeHelper p 3

-- it returns the first positive primes greater than x
nextPrime :: Int -> Int
nextPrime x
  | x == 2 = 3
  | even x = if prime' $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime' $ 2 + x then 2 + x else nextPrime $ 2 + x

-- it returns the sum of all primes under lim
sumPrimes :: Int -> Int
sumPrimes lim = helper 3 lim 2
  where helper i lim res
          | i >= lim = res
          | otherwise = helper (nextPrime i) lim (i + res)

-- it stores the value of primes in memory for faster access next time
primes = iterate nextPrime 2

-- it returns all primes less than n
primes_under :: Int -> [Int]
primes_under n = takeWhile (n > ) primes

div' :: (Integral t) => t -> t -> Bool
div' a m = (0 == rem a m)

pfactors :: Int -> [Int]
pfactors n = helper 2 n []
  where helper p n res
          | prime' n = n:res
          | div' n p = helper 2 (quot n p) (p:res)
          | otherwise = helper (nextPrime p) n res

-- Problem no 3 using pfactors took less than 10msec
-- Problem no 7 using sieve took less than 1msec

palin' n = res == reverse res
  where res = numcol n

numcol :: (Integral a) => a -> [Int]
numcol n = helper n []
  where helper :: (Integral a) => a -> [Int] -> [Int]
        helper i res
          | i < 10 = (fromIntegral i) : res
          | otherwise = helper (quot i 10)
                        ((fromIntegral (rem i 10)) : res)

palin3x3 = maximum [x*y| x <- [900..999], y <- [900..999], palin' (x*y)]

pita1000 = [a*b*c | a <- [1..250], b <- [a..500],
            let c = 1000 - b -a, a^2 + b^2 == c^2] 

-- Problem no 9 using smart list comprehension 10ms
                                            
triangle :: Int -> Int
triangle n = quot (n * (succ n)) 2

divisors :: Int -> [Int]
divisors 1 = [1]
divisors 2 = [1,2]
divisors n
  | even n = helperEven n 3 [1,2,(div n 2),n]
  | otherwise = helperOdd n 3 [1,n]
  where helperOdd :: Int -> Int -> [Int] -> [Int]
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then i : res
                       else helperOdd n (i + 2) (i : (div n i) : res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> [Int] -> [Int]
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then i : res
                       else helperEven n (succ i) (i: (div n i) : res)
          | otherwise = helperEven n (succ i) res

countDivs :: Int -> Int
countDivs 1 = 1
countDivs 2 = 2
countDivs n
  | even n = helperEven n 3 4
  | otherwise = helperOdd n 3 2
  where helperOdd :: Int -> Int -> Int -> Int
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then succ res
                       else helperOdd n (i + 2) (2 + res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> Int -> Int
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then succ res
                       else helperEven n (succ i) (2 + res)
          | otherwise = helperEven n (succ i) res

firstTriangle :: Int -> (Int, Int)
firstTriangle lim = head $ dropWhile (\x -> (fst x) < lim) materials
  where materials = map (\x -> (countDivs x, x)) $ map triangle [1..]

-- Problem no 12 elapsed time 0.66 secs

sumDivs :: Int -> Int
sumDivs 1 = 0
sumDivs 2 = 3
sumDivs n
  | even n = amicEven 
  | otherwise = amicOdd
  where amicEven = helperEven n 3 (2 + (div n 2) + 1)
        amicOdd = helperOdd n 3 1
        helperOdd :: Int -> Int -> Int -> Int
        helperOdd n i res
          | (i*i) > n = res
          | div' n i = if i == div n i
                       then i + res
                       else helperOdd n (i + 2) (i + (div n i) + res)
          | otherwise = helperOdd n (i + 2) res
        helperEven :: Int -> Int -> Int -> Int
        helperEven n i res
          | (i * i) > n = res
          | div' n i = if i == div n i
                       then i + res
                       else helperEven n (succ i) (i + (div n i) + res)
          | otherwise = helperEven n (succ i) res

amic' :: Int -> Bool
amic' n = (n == sumDivs amics) && n /= amics
  where amics = sumDivs n

-- Problem no 21 => 0.03 secs

distinct ls = map ( head . group . sort ) $ ls

frequencies ls = map (\x -> (head x, length x)) $ (group.sort) $ ls

check1 :: [(t,Int)] -> [(t,Int)]
check1 ls = filter (\x -> (snd x) == 1) ls

prob29 lim = Set.fromList [a^b| a <- [2..lim], b <- [2..lim]]

collatz :: Int -> (Int,Int)
collatz lim = maximumBy (comparing snd)  (map collas [1..lim])
  where collas :: Int -> (Int, Int)
        collas n = (n, colls n)
        colls :: Int -> Int
        colls n = if n == 1 then 1 else 1 + colls (calcol n)
        calcol :: Int -> Int
        calcol n = if even n then div n 2 else succ (3*n)

sol24 sx = (sort $ permutations "0123456789") !! sx        

sol30 :: Int -> Int
sol30 x = sum (filter required' [1..x])
  where sumfif :: Int -> Int
        sumfif p = sum $ map (\x -> x^5) (numcol p)
        required' :: Int -> Bool
        required' i = (i == sumfif i)
        
colnum :: (Integral a) => [a] -> a
colnum [] = 0
colnum ls = helper ls 0
  where helper (x:[]) res = x + res*10
        helper (x:xs) res = helper xs (x + res*10)

pandigital' ls = ( [1..9] == sort ls)
 
sol32 lim = sum $ distinct [c| a <- [1..lim], b <- [1..lim], a /= b, let c = a*b,
                            pandigital' $ (numcol a) ++ (numcol b) ++ (numcol c)]

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = product [1..n]

fact' :: Int -> Bool
fact' n = (n == sum (map fact (numcol n)))

-- it returns all possible cycle of a number n
circulars :: Int -> [Int]
circulars n = map (colnum.cycleit) [0..pred $ length ncol]
  where ncol = numcol n
        cycleit i = (drop i ncol) ++ (take i ncol)

-- it returns true if n is a circular prime
circularPrime' :: Int -> Bool
circularPrime' n
  | filtered n = False
  | otherwise = all prime' $ circulars n
  where ncol = numcol n
        filtered n = any existlah [0,2,4,5,6,8]
        existlah l = elem l ncol

sol35 lim = 2 + (length $ filter circularPrime' $ primes_under lim)

-- it returns the binary representation of n in a list
bincol :: Int -> [Int]
bincol n = bhelper n []
  where bhelper n res
          | n < 2 = n : res
          | otherwise = bhelper (div n 2) ((rem n 2) : res)

-- it returns true if n is a palindrome in decimal and binary bases
palin'' :: Int -> Bool
palin'' n = ((ncol == reverse ncol) && (nbin == reverse nbin))
  where ncol = numcol n
        nbin = bincol n

sol36 lim = sum $ filter palin'' [1..lim]










