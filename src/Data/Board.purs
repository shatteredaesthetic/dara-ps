module Data.Board where

import Prelude
import Data.Foldable (for_)
import Data.Coords (Coords(..))
import Data.Int (toNumber)
import Effect (Effect)
import Game (Grid, Cell)
import Graphics.Canvas (Arc, Context2D, arc, fillPath, setFillStyle, strokePath)
import Math as M

path :: Coords -> Arc
path (Coords x y) =
  { x: toNumber x, y: toNumber y, radius: 40.0, start: 0.0, end: M.pi * 2.0 }

circle :: Context2D -> Cell -> Effect Unit
circle ctx (Cell coords color) = void do
  let p = arc ctx $ path coords
  _ <- setFillStyle ctx $ show color
  _ <- fillPath ctx p
  strokePath ctx p

board :: Context2D -> Grid Cell -> Effect Unit
board ctx (Grid g) = void do
  for_ g \row ->
    for_ row \cell -> circle ctx cell
