module Main where

import Data.Traversable (for)

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.DOM

import Graphics.Canvas
import Debug.Trace

foreign import randomColor
    "function randomColor() {\
    \  return '#'+Math.floor(Math.random()*16777215).toString(16);\
    \}" :: forall eff. Eff (random :: Random | eff) String

strokeFillPath :: forall eff a. Context2D -> Eff (canvas :: Canvas | eff) a ->
                  Eff (canvas :: Canvas | eff) a
strokeFillPath ctx path = do
    fillPath ctx path
    strokePath ctx path

main = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    setStrokeStyle "#000000" ctx 

    node <- querySelector "#canvas"
    for node $ addEventListener "click" $ do
        trace "Mouse clicked!"

        color <- randomColor
        setFillStyle color ctx

        x <- random
        y <- random
        r <- random

        let path = arc ctx
             { x     : x * 600
             , y     : y * 600
             , r     : r * 50
             , start : 0
             , end   : Math.pi * 2
             }

        strokeFillPath ctx path

        return unit
