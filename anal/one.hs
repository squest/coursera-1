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
  | odd lls = merge' (msort (take (succ dls) ls)) (msort (drop (succ dls) ls)) []
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

main = do
  f <- readFile "raw.txt"
  let x = take 100000 $ fmap init $ lines f
  let y = map (\s -> map digitToInt s) x
  let z = map colnum y
  let a = inversion z
  return a

-- "Elapsed time 202 sec" answer:2407905288
               
  
      


