module Main where

import Prelude
import Data.Array.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type CellState
  = Empty
  | Black
  | White

instance showCellState :: Show CellState where
  show Empty = ""
  show Black = " black"
  show White = " white"

data Query a
  = Move Int a

type State =
  { board :: A.Array CellState
  , current :: CellState }

type Input = Unit

type Message = Unit

isBlack :: CellState -> Boolean
isBlack = (==) Black

isWhite :: CellState -> Boolean
isWhite = (==) White

chunks :: forall a. Int -> A.Array a -> A.Array (A.Array a)
chunks _ [] = []
chunks n xs = pure (NE.take n xs) <> (chunks n $ NE.drop n xs)

getRow :: Int -> A.Array a -> A.Array (A.Array a)
getRow idx arr = fromMaybe [] $ chunks 5 arr A.!! idx

getCol :: Int -> A.Array a -> A.Array (A.Array a)
getCol idx arr = flip A.index idx $ cols arr
  where
    rows = fromMaybe [] $ chunks 5 arr
    cols a = foldl A.snoc [] rows

validateArray :: A.Array a -> Boolean
validateArray arr =
  let
    Tuple a1 a2 = Tuple (Ne.tail arr) (NE.init arr)
    a = (all isBlack a1) && (all isWhite a1)
    b = (all isBlack a2) && (all isWhite a2)
  in
    a && b

validPlacement :: Int -> Int -> State -> Boolean
validPlacement x y state =
  let
    row = validateArray $ getRow x state
    col = validateArray $ getCol y state
  in
    row && col

board :: forall m. H.Component HH.HTML Query Input Message m
board =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState = { board: NE.replicate 30 Empty, current: Empty }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ "board" ]
        [ mapWithIndex
            (\i c -> HH.div
              [ HP.class_ "cell" <> show c
              , HE.onClick (HE.input_ Move i) ]
              [])
            state ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Move idx next -> do
        { board, current } <- H.get
        let newBoard = NE.updateAt idx current board
        let nextState =
