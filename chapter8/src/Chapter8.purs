module Chapter8 where

import Data.Maybe
import Data.Array
import Control.Monad
import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.ST
import Math

third :: forall a. [a] -> Maybe a
third xs = do
    t1 <- tail xs
    t2 <- tail t1
    head t2

sums :: [Number] -> [Number]
sums xs = sort $ nub $ foldM (\x y -> [x,y,x+y]) 0 xs

-- foldM f 0 (1 : [2,10])
-- a <- [0,1,1]
--   foldM f 0 (2 : [10])
--   a <- [0, 2, 2]
--     foldM f 0 (10 : [])
--     a <- [0, 10, 10]
--     foldM f 2 (10 : [])
--     a <- [2, 10, 12]
--     foldM f 2 (10 : [])
--     a <- [2, 10, 12]
--   foldM f 1 (2 : [10])
--   a <- [1, 2, 3]
--     foldM f 1 (10 : [])
--     a <- [1, 10, 11]
--     foldM f 2 (10 : [])
--     a <- [2, 10, 12]
--     foldM f 3 (10 : [])
--     a <- [3, 10, 13]
--   foldM f 1 (2 : [10])
--   a <- [1, 2, 3]
--     foldM f 1 (10 : [])
--     a <- [1, 10, 11]
--     foldM f 2 (10 : [])
--     a <- [2, 10, 12]
--     foldM f 3 (10 : [])
--     a <- [3, 10, 13]

filterM :: forall m a. (Monad m) => (a -> m Boolean) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = do
    x' <- p x
    if x'
       then (:) <$> return x <*> filterM p xs
       else filterM p xs

safeDivide :: Number -> Number -> Eff (err :: Exception) Number
safeDivide _ 0 = throwException $ error "Cannot divide by 0"
safeDivide num den = return (num / den)

simulatePi :: forall eff. Number -> Eff (random :: Random | eff) Number
simulatePi num = runST (do
    ref <- newSTRef { in: 0, tot: 0 }
    forE 0 num $ \i -> do
        p <- randomPoint
        let pdist = sqrt ( p.x * p.x + p.y * p.y )
        modifySTRef ref (\o ->
            { in: o.in + if (pdist <= 1) then 1 else  0
            , tot: o.tot + 1
            })
        return unit
    final <- readSTRef ref
    return (4 * final.in / final.tot))

 where
     randomPoint :: forall eff. Eff (random :: Random | eff) { x :: Number, y :: Number }
     randomPoint = do
         x <- random
         y <- random
         return { x: x, y: y }
