import Data.List

msort :: [Int] -> [Int]
msort [] = []
msort (x: []) = [x]
msort xs = merge (msort $ take n xs) (msort $ drop n xs)
  where n = div (length xs) 2
        merge l1 [] = l1
        merge [] l2 = l2
        merge (a:as) (b:bs)
          | a <= b = a : merge as (b:bs)
          | otherwise = b : merge (a:as) bs

datums :: [Int]
datums = [23,435,45,546,45,6456,4,53,45,34,23,4,234,34,5,345,4]

stupidInversion :: [Int] -> Int -> Int
stupidInversion xs size = looper 0 1 0
  where looper :: Int -> Int -> Int -> Int
        looper i j n 
          | i >= size = n
          | j > size = looper (1+i) (2+i) n
          | (xs !! j) < (xs !! i) = looper i (1+j) (1+n)
          | otherwise = looper i (1+j) n

main = do
  raw <- readFile "raw.txt"
  let ints = map (\x -> read x :: Int) $ words raw
      counter = stupidInversion ints 99999
  putStrLn $ show counter
