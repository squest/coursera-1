import Data.List

merge' as [] res = if null as then res else res ++ as
merge' (a:as) (b:bs) res
  | a < b = merge' as (b:bs) (res++ [a])
  | otherwise = merge' (a:as) bs (res++ [b])

msort [] = []
msort ls
  | odd $ length ls = m

