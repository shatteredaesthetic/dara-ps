module Game where

import Prelude
import Data.Array as A
import Data.Coords (Coords(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe)

data Color = Empty | Black | White

instance showColor :: Show Color where
  show Empty = "#717d85"
  show Black = "#000000"
  show White = "#ffffff"

data Cell = Cell Coords Color

data Grid a = Grid (Array (Array a))

derive instance functorGrid :: Functor Grid

makeGrid :: Color -> Grid Cell
makeGrid content =
  let
    f y x cnt = flip Cell cnt $ Coords x y
    g y r = flip mapWithIndex r $ f y
  in
    Grid $ mapWithIndex g $ A.replicate 5 $ A.replicate 6 content

changeGrid :: Cell -> Grid Cell -> Grid Cell
changeGrid cell@(Cell (Coords x y) color) (Grid grid) =
  let
    row = fromMaybe [] $ A.index grid y
    newRow = fromMaybe [] $ A.updateAt x cell row
  in
    fromMaybe [] $ A.updateAt y row grid
