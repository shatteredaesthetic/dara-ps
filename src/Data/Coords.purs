module Data.Coords where

import Prelude
import Data.Foldable (fold)
import Data.Int (quot, floor)

data Coords = Coords Int Int

instance showCoords :: Show Coords where
  show (Coords x y) = fold [ "Coords ", show x, " ", show y ]

derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords x y) = fold [ "(", show x, ", ", show y, ")" ]

adjust :: Number -> Int
adjust = ((+)50) <<< ((*)100) <<< flip quot 100 <<< floor

newCoords :: Number -> Number -> Coords
newCoords x y = Coords (adjust x) (adjust y)
