module Chapter5 where

import Data.Array
import Data.Picture
import Data.Maybe

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Number -> Number -> Number
binomial _ 0 = 1
binomial n k | k > n = 0
binomial n k = binomial (n - 1) (k - 1) + binomial (n - 1) k

allTrue :: [Boolean] -> Boolean
allTrue [] = true
allTrue (false : rest) = false
allTrue (true : rest) = allTrue rest

isSorted :: [Number] -> Boolean
isSorted [] = true
isSorted [_] = true
isSorted (x : y : rest) = x <= y && isSorted (y : rest)

type Address = { city :: String }
type Person = { address :: Address }

getCity :: Person -> String
getCity { address = { city = c } } = c

flatten :: forall a. [[a]] -> [a]
flatten [] = []
flatten (hd : tl) = hd ++ flatten tl

makeCircle :: Shape
makeCircle = Circle (Point { x: 0, y: 0 }) 10

scale :: Shape -> Shape
scale = scale'
  where
      scalePoint :: Point -> Point
      scalePoint (Point { x = x, y = y }) = Point { x: 2*x, y: 2*y }

      scale' :: Shape -> Shape
      scale' (Circle p n) = Circle (scalePoint p) (2 * n)
      scale' (Rectangle p w h) = Rectangle (scalePoint p) (2*w) (2*h)
      scale' (Line p1 p2) = Line (scalePoint p1) (scalePoint p2)
      scale' (Text p s) = Text (scalePoint p) s

extractText :: Shape -> Maybe String
extractText (Text _ s) = Just s
extractText _ = Nothing
