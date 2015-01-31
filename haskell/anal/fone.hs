import Data.List

datums :: [Int]
datums = [1,3,2,4,3,4,345,345,45,64,6,35,456,4,634,545,76,78,234,53,45]

msort :: [Int] -> [Int]
msort [] = []
msort (x: []) = [x]
msort xs = merge (msort $ take lxs xs) (msort $ drop lxs xs)
  where lxs = div (length xs) 2
        merge l1 [] = l1
        merge [] l2 = l2
        merge (a:as) (b:bs)
          | a <= b = [a] ++ merge as (b:bs)
          | otherwise = [b] ++ merge (a:as) bs

inverse :: [Int] -> (Int, [Int])
inverse [] = (0,[])
inverse (x: []) = (0, [x])
inverse xs = merge (inverse $ take n xs) (inverse $ drop n xs)
  where n = div (length xs) 2
        merge :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
        merge (res1, l1) (res2,[]) = (res1 + res2, l1)
        merge (res1, []) (res2, l2) = (res1 + res2, l2)
        merge (res1, (a:as)) (res2, (b:bs))
          | a <= b = merge (res1, as) (res2, (b:bs))
          | otherwise = merge (res1+1,(a:as)) (res2,bs)
        
main = do
  well <- readFile "IntegerArray.txt"
  let raw = map (\x -> read x :: Int) $ words well
      count_inversion = inverse raw
  putStrLn $ show count_inversion



