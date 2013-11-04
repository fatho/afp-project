module Logic.TicTacToe 
  ( TicTacToe (..)
  , Board, Pos, Cell
  , Player (..)
  , newGame, move
  , switch
  , emptyBoard, boardAt, serializeBoard, deserializeBoard
  , serializeGameState, deserializeGameState
  ) where

import Prelude
import Control.Applicative
import Data.Array

data Player    = PlayerX | PlayerO deriving (Show, Eq, Ord, Bounded, Enum)
type Cell      = Maybe Player
type Pos       = (Int,Int)
newtype Board  = Board (Array Pos Cell) deriving Eq
data TicTacToe = TicTacToe { board :: Board, currentPlayer :: Player } deriving (Show, Eq)

instance Show Board where
  show = serializeBoard

instance Read Board where
  readsPrec _ str = case deserializeBoard prefix of
      Nothing -> []
      Just  b -> [(b, rest)]
    where
      (prefix, rest) = splitAt 9 str

switch :: Player -> Player
switch PlayerX = PlayerO
switch PlayerO = PlayerX  

emptyBoard :: Board
emptyBoard = Board $ listArray ((1,1),(3,3)) (repeat Nothing)

newGame :: TicTacToe
newGame = TicTacToe emptyBoard PlayerX

boardAt :: Board -> Pos -> Cell
boardAt (Board b) p = b ! p

move :: TicTacToe -> Pos -> TicTacToe
move (TicTacToe (Board curBoard) player) pos = TicTacToe (Board $ curBoard // [(pos, Just player)]) (switch player)

serializeGameState :: TicTacToe -> String
serializeGameState (TicTacToe f p) = psym p : serializeBoard f where
  psym PlayerX = 'X'
  psym PlayerO = 'O'

deserializeGameState :: String -> Maybe TicTacToe
deserializeGameState (p:fstr) = TicTacToe <$> deserializeBoard fstr <*> symToPlayer p where
  symToPlayer 'X' = Just PlayerX
  symToPlayer 'O' = Just PlayerO
  symToPlayer  _  = Nothing
deserializeGameState _        = Nothing


serializeBoard :: Board -> String
serializeBoard (Board fld) = map cellToChar $ elems fld where
  cellToChar Nothing        = '~'
  cellToChar (Just PlayerX) = 'X'
  cellToChar (Just PlayerO) = 'O'

deserializeBoard :: String -> Maybe Board
deserializeBoard str = Board . listArray ((1,1),(3,3)) <$> sequence paddedInput where
  charToCell '~' = Just Nothing
  charToCell 'X' = Just (Just PlayerX)
  charToCell 'O' = Just (Just PlayerO)
  charToCell  _  = Nothing
  -- Eingabestring muss 9 Zeichen lang sein. Es werden die ersten 9 genommen. 
  -- Sind es weniger als 9, wird mit Nothing aufgefüllt (was in sequence den Gesamtausdruck zu Nothing werden lässt)
  paddedInput = take 9 (map charToCell str ++ repeat Nothing)
