module Main where

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)
import Debug.Trace
import Data.Array
import Math

type Point = { x :: Number, y :: Number }

showPoint :: Point -> String
showPoint { x = x, y = y } = "{" ++ show x ++ ", " ++ show y ++ "}"

renderPath :: forall eff. Context2D -> [Point] ->
              Eff (canvas :: Canvas | eff) Context2D
renderPath ctx [] = return ctx
renderPath ctx (hd : tl) = do
    setFillStyle "#00FF00" ctx

    fillPath ctx $ do
        moveTo ctx hd.x hd.y
        foreachE tl $ \p -> do
            lineTo ctx p.x p.y
            return unit
        closePath ctx

    return ctx

sampleFunction :: (Number -> Point) -> Number -> [Point]
sampleFunction _ 0 = []
sampleFunction f n = sampleFunction' [] 1 (1.0 / (n - 1)) f n
  where
      sampleFunction' :: [Point] -> Number -> Number -> (Number -> Point) -> Number -> [Point]
      sampleFunction' acc _ _ _ 0 = acc
      sampleFunction' acc cur delta f n =
          sampleFunction' ((f cur) : acc) (cur - delta) delta f (n - 1)

renderFunction :: forall eff. Context2D -> (Number -> Point) ->
                  Eff (canvas :: Canvas | eff) Context2D
renderFunction ctx f = renderPath ctx $ sampleFunction f 100

main = do
    canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    --renderPath ctx [{x: 10, y: 10}, {x: 110, y: 10}, {x: 110, y: 110}]
    --renderFunction ctx (\i -> {x: 200 + 100 * cos (pi * i), y: 200 + 100 * sin (pi * i)})
    renderFunction ctx (\i -> {x: 100 + 300 * i, y: 100 + 100 * sin (10 * pi * i)})

    setFillStyle "#0000FF" ctx

    fillPath ctx $ do
        let r1 = {x: 250, y: 450, w: 100, h: 100}
        let r2 = {x: 355, y: 450, w: 100, h: 100}
        rect ctx r1
        rect ctx r2
