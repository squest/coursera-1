module Math where

import Data.List

primeHelper :: Int -> Int -> Bool
primeHelper p i
  | (i*i) > p = True
  | 0 == (rem p i) = False
  | otherwise = primeHelper p (i + 2)

-- it returns true if p is prime and false otherwise
prime :: Int -> Bool
prime p
  | p <= 10 = elem p [2,3,5,7]
  | even p = False
  | otherwise = primeHelper p 3


-- it returns the first positive primes greater than x
nextPrime :: Int -> Int
nextPrime x
  | x == 2 = 3
  | even x = if prime $ succ x then succ x else nextPrime $ succ x
  | otherwise = if prime $ 2 + x then 2 + x else nextPrime $ 2 + x

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
          | prime n = n:res
          | div' n p = helper 2 (quot n p) (p:res)
          | otherwise = helper (nextPrime p) n res



                                            
