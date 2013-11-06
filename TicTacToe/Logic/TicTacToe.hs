module Logic.TicTacToe 
  ( TicTacToe (..)
  , Board, Pos, Cell
  , Player (..)
  , newGame, move
  , possibleMoves
  , switch
  , emptyBoard, boardAt, serializeBoard, deserializeBoard
  , serializeGameState, deserializeGameState
  , outcome
  ) where

import Prelude
import Control.Applicative
import Data.Array
import Data.Maybe
import Data.List
import Data.Function

import Logic.MiniMax

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

instance Ord TicTacToe where
  compare = compareO `on` outcome

compareO :: (Maybe (Maybe Player)) -> (Maybe (Maybe Player)) -> Ordering
compareO Nothing (Just (Just PlayerX)) = LT
compareO Nothing (Just (Just PlayerO)) = GT
compareO Nothing _                     = EQ

compareO (Just (Just PlayerX)) (Just (Just PlayerX)) = EQ
compareO (Just (Just PlayerX)) _                     = GT

compareO (Just (Just PlayerO)) (Just (Just PlayerO)) = EQ
compareO (Just (Just PlayerO)) _                     = LT

compareO (Just Nothing) (Just (Just PlayerX)) = LT
compareO (Just Nothing) (Just (Just PlayerO)) = GT
compareO (Just Nothing) _                     = EQ

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

possibleMoves :: TicTacToe -> [Pos]
possibleMoves (TicTacToe (Board b) _) = filter (\p -> isNothing $ b!p) $ range ((1,1),(3,3))

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


-- Test darauf, ob das Spiel beendet ist, und mit welchem Ergebnis:
--   outcome field = Nothing       ==>  Spiel kann noch fortgesetzt werden
--   outcome field = Just Nothing  ==>  Spiel ist beendet, mit Unentschieden
--   outcome field = Just (Just player), wobei entweder player == xPlayer 
--                                                 oder     player == oPlayer
--                                     ==>  Spiel ist beendet, der zureckgelieferte
--                                          Spieler hat gewonnen
-- (Sie duerfen annehmen, dass nur echt im Spielverlauf erreichbare Situationen
--  ueberprueft werden, insbesondere keine Spielfelder, in denen beide Spieler
--  je alle drei Positionen in einer Reihe, Spalte oder Hauptdiagonale besetzt
--  haben.)
outcome :: TicTacToe -> Maybe (Maybe Player)
outcome (TicTacToe (Board field) _) = maximumBy cord $ map checkCondition (conditions field)
  --where
-- Reduziert die Ergebnisse der Einzelbedingungen auf ein Ergebnis

-- Definiert auf den Einzelergenissen eine Ordnung
cord :: Maybe (Maybe Player) -> Maybe (Maybe Player) -> Ordering
cord a b = rank a `compare` rank b where
  rank (Just Nothing) = 0
  rank Nothing        = 1
  rank _              = 2

-- Reduziert eine Gewinnbedingung auf den Spieler, wenn alle drei Zellen vom 
-- gleichen Spieler markiert sind, oder auf Nothing, wenn dies nicht der Fall ist.
checkCondition :: [Cell] -> Maybe (Maybe Player)
checkCondition xs | any (==Nothing) xs = Nothing
                  | otherwise          = Just (foldl1 equalOrNothing xs)

-- Liste aller Zellen entlang der Gewinnbedingungen
conditions field = map (map (field!)) conditionsPos
-- Liste aller Dreierkombinationen von Positionen, die zum Gewinn führen
conditionsPos :: [[Pos]]
conditionsPos =  [ (,) <$> [r] <*> [1..3] | r <- [1..3] ] -- alle Zeilen
              ++ [ (,) <$> [1..3] <*> [c] | c <- [1..3] ] -- alle Spalten
              ++ [ zip [1..3] [1..3]
                 , zip [3,2,1] [1..3] ]           -- Hauptdiagonalen

-- der Equal-Or-Nothing Operator
equalOrNothing :: (Eq a) => Maybe a -> Maybe a -> Maybe a
equalOrNothing Nothing      _                    = Nothing
equalOrNothing _            Nothing              = Nothing
equalOrNothing val@(Just x) (Just y) | x == y    = val
                                     | otherwise = Nothing

