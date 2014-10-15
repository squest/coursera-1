import Data.List

n = a `div` length xs
    where a = 10
          xs = [1..5]

-- last' xs = xs !! (length xs -1)

last' xs = drop (length xs - 1) xs

sam = [1..9]

      
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [n|n<-xs, n < x]
        larger = [n|n<-xs, n > x]


qsort' [] = []
qsort' xs = qsort larger ++ qsort smaller ++ [x]
  where x = minimum xs
        smaller = [n|n<-xs, n <= x]
        larger = [n|n <- xs, n > x]


      
qsort [] = []
qsort (x:xs) = reverse (reverse (qsort smaller) ++ [x] ++ reverse (qsort larger))
  where smaller = [n|n<-xs, n <= x]
        larger = [n|n<-xs, n > x]



qsort [] = []
qsort xs = x : qsort larger ++ qsort smaller
  where x = maximum xs
        smaller = [n|n<-xs, n < x]
        larger = [n|n<-xs, n >= x]



