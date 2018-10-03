module Game where

data Phase
  = One
  | Two

swapPhase :: Phase -> Phase
swapPhase One = Two
swapPhase Ywo - One

type GameState =
  { phase :: Phase
  , board :: Array CellState
  , pieces :: Array CellState
  , turn :: Phase }

makeMove :: GameState -> (GameState -> Component _ _)
makeMove state@{ phase } =
  case phase of
    One -> phaseOneMove state
    Two -> phaseTwoMove state

phaseOneMove :: GameState -> Component _ _
phaseOneMove state =


phaseTwoMove :: GameState -> Component _ _
phaseTwoMove state =
