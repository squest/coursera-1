import Data.List
import System.IO

our_data = [43,34,5,46,56,465,53,5,45,5,34,43,45,56,57,67]
foo_data = our_data ++ reverse our_data ++ our_data
bar_data = foo_data ++ reverse foo_data ++ foo_data
brutal = concatMap (\x -> [x, rem x 2, pred x, mod x 5]) bar_data

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = smaller xs ++ [x] ++ larger xs
  where tmp = qsort xs
        smaller xs = takeWhile (<= x) tmp
        larger xs = dropWhile (<= x) tmp

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x: []) = [x]
msort xs = merge (msort $ take n xs) (msort $ drop n xs)
  where n = div (length xs) 2
        merge l1 [] = l1
        merge [] l2 = l2
        merge (a:as) (b:bs)
          | a <= b = [a] ++ merge as (b:bs)
          | otherwise = [b] ++ merge (a:as) bs

myReverse :: [Char] -> [Char]
myReverse = reverse

main = do
  contents <- readFile "data.txt"
  let what = words contents
      that = map (\x -> read x :: Int) what
      this = msort that
  putStrLn $ show this
