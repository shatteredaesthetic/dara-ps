module Dara.Board (board) where

import Prelude

import Turbine (runComponent)
import Board (board)

main :: Effect Unit
main = runComponent "#mount" board
