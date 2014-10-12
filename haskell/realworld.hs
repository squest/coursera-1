import Data.List
import Data.Maybe
import Data.Ord
import Data.Function

lastButOne :: [a] -> a
lastButOne = last.init

data BookInfo = Book Int String [String]
                deriving (Show)

data NumberInfo = Number Int Bool
                  deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList Nil = []
toList (Cons x Nil) = [x]
toList (Cons x xs) = x : (toList xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


qSecond [] = error "Guoblog!!"
qSecond (_: []) = error "Second mesti lebih dari 1 element yaa"
qSecond (_:x:xs) = x

safeSecond :: [a] -> Maybe a
safeSecond (_:x:xs) = Just x
safeSecond _ = Nothing

qLength :: [a] -> Int
qLength [] = 0
qLength (_:xs)
  | null xs = 1
  | otherwise = succ $ qLength xs

avgOfElmt :: (Integral a) => [a] -> Float
avgOfElmt [] = 0
avgOfElmt ls = (fromIntegral $ sum ls) / (fromIntegral $ qLength ls)

palindrome :: [a] -> [a]
palindrome ls = ls ++ reverse ls

palin' :: Eq a => [a] -> Bool
palin' ls = (ls == reverse ls)

sortLength :: [[a]] -> [[a]]
sortLength ls = sortBy (on compare length) ls

qIntersperse a [] = []
qIntersperse a (c:d:xs) = c++ [a] ++ (qIntersperse a (d:xs))
qIntersperse a (l: []) = l

lChild :: Tree a -> Tree a
lChild (Node _ lc _) = lc

rChild :: Tree a -> Tree a
rChild (Node _ _ rc) = rc

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ Empty Empty) = 1
treeDepth ls = maximum $
               [succ $ treeDepth (lChild ls)] ++ [succ $ treeDepth (rChild ls)] 

aTree = Node "parent" Empty Empty
bTree = Node "parent" aTree Empty
cTree = Node "parent" bTree Empty
dTree = Node "parent" Empty aTree
eTree = Node "parent" Empty cTree


