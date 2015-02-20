module Main where

import Control.Monad.Eff
import Control.Monad.Eff.DOM (DOM())

import Debug.Trace

import Data.AddressBook.UI

import Chapter8
import Control.Monad.Eff.Random

main :: forall eff. Eff (random :: Random, trace :: Trace, dom :: DOM | eff) Unit
main = do
    --pi <- simulatePi 10000
    --print pi
    trace "Attaching event handlers"
    setupEventHandlers 
