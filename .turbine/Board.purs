module Board (board) where

import Prelude

import Control.Apply (lift2)
import Data.Array.NonEmpty as NE
import Hareactive.Types (Behavior, Stream, Now)
import Hareactive.Combinators (sample, scan)
import Turbine (Component, runComponent, list, modelView, output)
import Turbine.HTML.Elements as E

data CellState
  = Empty
  | Black
  | White

type CellOut =
  { inner :: Behavior CellState }

type CellViewOut =
  { select :: Stream Unit }

cellModel :: CellViewOut -> Int -> CellState -> (Now CellOut)
cellModel { select } id state = do
  inner <- sample $ scan identity Empty (select #> state)
  pure { inner }

cellView :: CellOut -> Int -> CellState -> Component CellViewOut CellViewOut
cellView { inner } id _ = do
  let
    stateClass :: CellState -> String
    stateClass state = case state of
      Black -> "black"
      White -> "white"
      Empty -> "empty"
    meta = { class: E.staticClass "cell " <> stateClass inner }
  E.button meta (E.text $ show id) `output` (\o -> { select: o.click })

cell :: CellState -> Component {} CellOut
cell = modelView cellModel cellView

type BoardOut =
  { cells :: Behavior (Array CellState) }

type BoardViewOut =
  { listOut :: Behavior (Array CellOut)}

boardModel :: BoardViewOut -> CellState -> Now BoardOut
boardModel { listOut } tile = do
  let init = replicate 30 Empty
  cells <- sample $ scan validate init listOut
  pure { cells, current: tile }

boardView :: BoardOut -> CellState -> Component _ BoardViewOut
boardView { cells, current } _ =
  E.div { class: E.staticClass "board" } (
    list (\id -> cell id current `output` identity) cells identity `output` (\o -> { listOut: o })
  )

board :: CellState -> Component {} BoardOut
board = modelView boardModel boardView Black
