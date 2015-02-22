module Main where

import Data.Maybe
import Data.Either
import Data.Array (map)
import Data.String (joinWith, length)

import Control.Monad.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras
import Control.Monad.Error.Trans
import Control.Monad.Parallel

import Network.HTTP.Client

import Debug.Trace

import Files
import Math
import Data.Function

type Milliseconds = Number

foreign import data Timeout :: !

foreign import setTimeoutImpl """
    function setTimeoutImpl(t, f) {
        return function () {
            setTimeout(f, t);
            return {};
        };
    }
""" :: forall eff. Fn2 Milliseconds
                       (Eff (timeout :: Timeout | eff) Unit)
                       (Eff (timeout :: Timeout | eff) Unit)

setTimeout_ :: forall eff. Milliseconds -> (Unit -> Eff (timeout :: Timeout | eff) Unit) -> Eff (timeout :: Timeout | eff) Unit
setTimeout_ ms f = runFn2 setTimeoutImpl ms (f $ unit)

setTimeoutCont :: forall eff. Milliseconds -> ContT Unit (Eff (timeout :: Timeout | eff)) Unit
setTimeoutCont ms = ContT $ setTimeout_ ms

timeout :: forall a eff. Milliseconds ->
           ContRef (timeout :: Timeout | eff) a ->
           ContRef (timeout :: Timeout | eff) (Maybe a)
timeout ms c =
    race (setTimeoutCont ms >>= \_ -> return Nothing)
         (c >>= \a -> return $ Just a)

purescript_org :: Request
purescript_org = Request 
   { host: "www.purescript.org"
   , path: "/" 
   }

slashdot_org :: Request
slashdot_org = Request
    { host: "www.slashdot.org"
    , path: "/"
    }

responseToString :: Response -> String
responseToString (Response chunks) = joinWith "" $ map runChunk chunks

getResponseText :: forall eff. Request -> ContT Unit (WithHTTP (ref :: Ref | eff)) String
getResponseText req = responseToString <$> getAll req

getResponseLength :: forall eff. Request -> ContT Unit (WithHTTP (ref :: Ref | eff)) Number
getResponseLength req = foldC f 0 (getCont req)
  where
      f n Nothing = Right n
      f n (Just chunk) = Left (n + (length $ runChunk chunk))

main = flip runContT print $
       timeout 200 (getResponseText slashdot_org)

main10 = flip runContT print $
         race (getResponseText purescript_org) (getResponseText slashdot_org)

main9 = flip runContT trace $
        par (++) (getResponseText purescript_org)
                 (getResponseText slashdot_org)

main8 = flip runContT print $ getResponseLength purescript_org

main7 = runContT (getResponseText purescript_org) $
        \contents -> runContT (runErrorT $ writeFileContErr "out.txt" contents) print

main6 = runContT (runErrorT $ getContErr purescript_org) print
  where
      purescript_org :: Request
      purescript_org = Request 
        { host: "www.purescript.org"
        , path: "/"
        }

main5 = runContT (getCont purescript_org) print

main4 = runContT (runErrorT $ concatFilesContErr ["tmp1.txt", "tmp2.txt"] "tmp3.txt") print

main3 = runContT (setTimeoutCont 1000) print

main2 = runContT (concatFilesCont ["tmp1.txt", "tmp2.txt"] "tmp3.txt") print

main1 = runContT (getResponseText purescript_org) trace
