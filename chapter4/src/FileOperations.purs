module FileOperations where

import Data.Path
import Data.Array
import Data.Foldable
import Data.Maybe
import Control.MonadPlus

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child

isEven :: Number -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven $ n - 2

countEven :: [Number] -> Number
countEven [] = 0
countEven xs = countEven' 0 xs
  where
      countEven' :: Number -> [Number] -> Number
      countEven' x [] = x
      countEven' x xs = countEven' (x + (1 - (hd % 2))) tl
        where
            hd = Data.Array.Unsafe.head xs
            tl = Data.Array.Unsafe.tail xs

squares :: [Number] -> [Number]
squares xs = (\n -> n * n) <$> xs

removeNegative :: [Number] -> [Number]
removeNegative xs = filter (\n -> n >= 0) xs

(<$?>) :: forall a. (a -> Boolean) -> [a] -> [a]
(<$?>) = filter

factors :: Number -> [[Number]]
factors n = filter (\xs -> product xs == n) $ do
    i <- 1 .. n
    j <- i .. n
    return [i, j]

isPrime :: Number -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: forall a. [a] -> [a] -> [[a]]
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    return [x, y]

triples :: Number -> [[Number]]
triples n = do
    a <- 1 .. n
    b <- a .. n
    c <- b .. n
    guard $ a * a + b * b == c * c
    return [a, b, c]

-- TODO: factors using Foldable.any (4.11 ex 4)
-- TODO: factorization (4.11 ex 5)

allTrue :: [Boolean] -> Boolean
allTrue = foldl (==) true

countTCO :: forall a. (a -> Boolean) -> [a] -> Number
countTCO = countTCO' 0
  where
      countTCO' acc _ [] = acc
      countTCO' acc p (x : xs) = if p x then countTCO' (1 + acc) p xs else countTCO' acc p xs

onlyFiles :: Path -> [Path]
onlyFiles file = filter (not <<< isDirectory) (allFiles file)

findWinningChild :: (Path -> Path -> Path) -> Path -> Maybe Path
findWinningChild p path = findWinningChild' $ onlyFiles path
  where
      findWinningChild' :: [Path] -> Maybe Path
      findWinningChild' [] = Nothing
      findWinningChild' (x : xs) = Just $ foldl p x xs

findLargestChild :: Path -> Maybe Path
findLargestChild = findWinningChild (\p1 p2 -> if (size p1) > (size p2) then p1 else p2)

findSmallestChild :: Path -> Maybe Path
findSmallestChild = findWinningChild (\p1 p2 -> if (size p1) < (size p2) then p1 else p2)

whereIs :: String -> Maybe Path
whereIs = head <<< whereIs' root
  where
      whereIs' :: Path -> String -> [Path]
      whereIs' path name = do
          child <- ls path
          if (filename child == name)
             then [child]
             else whereIs' child name
