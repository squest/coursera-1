import Data.List
import Data.List.Split
import System.IO
import Data.Char


merge' as [] res = if null as then res else res ++ as
merge' [] bs res = res ++ bs
merge' (a:as) (b:bs) res
  | a < b = merge' as (b:bs) (res ++ [a])
  | otherwise = merge' (a:as) bs (res ++ [b])

msort [] = []
msort ls
  | lls == 1 = ls
  | odd lls = merge' (msort (take (succ dls) ls))
              (msort (drop (succ dls) ls)) []
  | even lls = merge' (msort (take dls ls)) (msort (drop dls ls)) []
  where lls = length ls
        dls = div lls 2

qsort [] = []
qsort (x:xs) = (qsort smaller) ++ [x] ++ (qsort larger)
  where smaller = filter (<=x) xs
        larger = filter (>x) xs

colnum ls = helper ls 0
  where helper (x:xs) res
          | null xs = x + 10*res
          | otherwise = helper xs (x+10*res)

inversion (x: []) = 0
inversion (x:xs) = res + (length (filter (< x) xs))
  where res = inversion xs

sum_primes :: Int -> Int
sum_primes lim = looper 3 2
  where looper p res
          | p > lim = res
          | prime 3 = looper (p+2) (res + p)
          | otherwise = looper (p+2) res
          where prime i
                  | i*i > p = True
                  | 0 == rem p i = False
                  | otherwise = prime $ i + 2


main = do
  raw <- readFile "IntegerArray.txt"
  let a = take 10 $ map (\x -> read x :: Int) $ words raw
  putStrLn $ show a

               
  
      
