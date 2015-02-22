module Control.Monad.Parallel where

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Cont.Extras

import Control.Monad.Cont.Trans

--type WithRef eff = Eff (ref :: Ref | eff)

--type ContRef eff = ContT Unit (WithRef eff)

par :: forall a b r eff. (a -> b -> r) -> 
         ContRef eff a -> ContRef eff b -> ContRef eff r
par f ca cb = ContT $ \k -> do
  ra <- newRef Nothing
  rb <- newRef Nothing
  runContT ca $ \a -> do
    mb <- readRef rb
    case mb of
      Nothing -> writeRef ra $ Just a
      Just b -> k (f a b)
  runContT cb $ \b -> do
    ma <- readRef ra
    case ma of
      Nothing -> writeRef rb $ Just b
      Just a -> k (f a b)

race :: forall a eff. ContRef eff a -> ContRef eff a -> ContRef eff a
race c1 c2 = ContT $ \k -> do
    rdone <- newRef false
    runContT c1 $ \a -> returnWinner rdone k a
    runContT c2 $ \a -> returnWinner rdone k a
  where
      returnWinner :: RefVal Boolean -> (a -> WithRef eff Unit) -> a -> WithRef eff Unit
      returnWinner rdone k a = do
          mdone <- readRef rdone
          case mdone of
               false -> do
                   writeRef rdone true
                   k a
               true -> return unit

newtype Parallel eff a = Parallel (ContRef eff a)

runParallel :: forall eff a. Parallel eff a -> ContRef eff a
runParallel (Parallel c) = c

instance functorParallel :: Functor (Parallel eff) where
  (<$>) f (Parallel c) = Parallel (f <$> c)

instance applyParallel :: Apply (Parallel eff) where
  (<*>) (Parallel f) (Parallel x) = Parallel (par ($) f x)

instance applicativeParallel :: Applicative (Parallel eff) where
  pure a = Parallel $ pure a


