module Main where

import Prelude
import Data.Board (Color(..),Cell(..), board, makeGrid, setCell)
import Data.Coords (Coords(..), newCoords)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.DOM (addEventListener, querySelector)
import Effect.Ref (read, modify_, new)
import Game (changeGrid)
import Graphics.Canvas (getCanvasElementById, getContext2D,
                        setCanvasHeight, setCanvasWidth,setStrokeStyle)
import Partial.Unsafe (unsafePartial)

type CanvasConfig =
  { width :: Number
  , height :: Number
  , color :: String }

makeCanvas :: CanvasConfig -> Effect Context2D
makeCanvas cfg = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  _ <- setCanvasWidth canvas cfg.width
  _ <- setCanvasHeight canvas cfg.height
  ctx <- getContext2D canvas
  _ <- setStrokeStyle ctx cfg.color
  pure ctx

main :: Effect Unit
main = void $ unsafePartial do
  let cfg = { width: 600.0, height: 500.0, color: "000" }
  ctx <- makeCanvas cfg

  grid <- new $ makeGrid Empty
  board ctx grid

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ \x y -> do
    let newCell = Cell (newCoords x y) Black
    modify_ (\g -> g <#> changeGrid newCell) grid
    newGrid <- read grid
    board ctx newGrid
