module Math where

import Data.List

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


                                            













