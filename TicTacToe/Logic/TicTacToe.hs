module Logic.TicTacToe 
  ( TicTacToe (..)
  , Board, Pos, Cell
  , Player (..)
  , newGame, move
  , switch
  , emptyBoard, serializeBoard, deserializeBoard
  ) where

import Prelude
import Control.Applicative
import Data.Array

data Player    = PlayerX | PlayerO deriving (Show, Eq, Ord, Bounded, Enum)
type Cell      = Maybe Player
type Board     = Array Pos Cell
type Pos       = (Int,Int)
data TicTacToe = TicTacToe { board :: Board, currentPlayer :: Player } deriving (Show, Eq)

switch :: Player -> Player
switch PlayerX = PlayerO
switch PlayerO = PlayerX  

emptyBoard :: Board
emptyBoard = listArray ((1,1),(3,3)) (repeat Nothing)

newGame :: TicTacToe
newGame = TicTacToe emptyBoard PlayerX

move :: TicTacToe -> Pos -> TicTacToe
move (TicTacToe currentBoard player) pos = TicTacToe (currentBoard // [(pos, Just player)]) (switch player)

serializeBoard :: Board -> String
serializeBoard fld = map cellToChar $ elems fld where
  cellToChar Nothing        = '~'
  cellToChar (Just PlayerX) = 'X'
  cellToChar (Just PlayerO) = 'O'

deserializeBoard :: String -> Maybe Board
deserializeBoard str = listArray ((1,1),(3,3)) <$> sequence paddedInput where
  charToCell '~' = Just Nothing
  charToCell 'X' = Just (Just PlayerX)
  charToCell 'O' = Just (Just PlayerO)
  charToCell  _  = Nothing
  -- Eingabestring muss 9 Zeichen lang sein. Es werden die ersten 9 genommen. 
  -- Sind es weniger als 9, wird mit Nothing aufgefüllt (was in sequence den Gesamtausdruck zu Nothing werden lässt)
  paddedInput = take 9 (map charToCell str ++ repeat Nothing)
