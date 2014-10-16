import Data.List

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

