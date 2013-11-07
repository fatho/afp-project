-- Based on solution of Lukas Heidemann

module Logic.TicTacToe
  ( Player (..)
  , TicTacToe (..)
  , GameInfo (..)
  , Pos
  , initialField
  , getField
  , setField
  , moves
  , winner
  , player
  , positions
  , freePositions
  , hasEnded
  , serializeField
  , deserializeField
  , gameInfo
  , switch
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits hiding (Bits)
import Data.Maybe
import Prelude

--------------------------------------------------------------------------------
-- * Tic Tac Toe

-- | Tic tac toe player.
data Player = PlayerX | PlayerO deriving (Eq, Show, Read)

-- | Representation of the current game state. Not needed by actual AI.
data GameInfo = WonBy Player | Draw | InProgress deriving (Show)
hasEnded :: GameInfo -> Bool
hasEnded InProgress = False
hasEnded _          = True

-- | Bit field.
type Bits = Int

-- | Tic tac toe game state; first bitset for PlayerX, second for PlayerO.
data TicTacToe = TicTacToe !Bits !Bits deriving (Eq)

-- | Position on the game field.
type Pos = (Int, Int)

instance Ord TicTacToe where
  compare f f' = compare (value f) (value f') where
    value field = case winner field of
      Just PlayerX  -> -1
      Just PlayerO  -> 1
      Nothing -> 0

--------------------------------------------------------------------------------
-- ** Pretty printing

instance Show TicTacToe where
  show = serializeField

instance Read TicTacToe where
  readsPrec _ str = case deserializeField prefix of
      Nothing -> []
      Just  b -> [(b, rest)]
    where
      (prefix, rest) = splitAt 9 str

-- | Returns the row major string representaion of the tic tac toe field.
-- The players are encoded by 'X' and 'O', empty cells by '~'
serializeField :: TicTacToe -> String
serializeField field = fmap (toChar . getField field) positions where
    toChar (Just PlayerX) = 'X'
    toChar (Just PlayerO) = 'O'
    toChar Nothing  = '~'

-- | Creates a tic tac toe field from a row major string representation.
-- The players are encoded by 'X' and 'O', empty cells by '~'
deserializeField :: String -> Maybe TicTacToe
deserializeField str = foldl set initialField <$> zip positions <$> mapM fromChar input where
  input = take 9 (str ++ repeat '~')
  fromChar 'X' = Just (Just PlayerX)
  fromChar 'O' = Just (Just PlayerO)
  fromChar '~' = Just Nothing
  fromChar _   = Nothing
  set field (pos, (Just p)) = setField field p pos
  set field (_, Nothing)    = field

--------------------------------------------------------------------------------
-- ** Operations

initialField :: TicTacToe
initialField = TicTacToe 0 0

-- | Updates a position of the field.
setField :: TicTacToe -> Player -> Pos -> TicTacToe
setField (TicTacToe x o) PlayerX pos = TicTacToe (setBit x $ posToBit pos) o
setField (TicTacToe x o) PlayerO pos = TicTacToe x (setBit o $ posToBit pos)

-- | Checks a position on the field.
getField :: TicTacToe -> Pos -> Maybe Player
getField (TicTacToe x o) pos
  | testBit x $ posToBit pos = Just PlayerX
  | testBit o $ posToBit pos = Just PlayerO
  | otherwise                = Nothing

-- | All possible moves of the current player.
moves :: TicTacToe -> [TicTacToe]
moves field
  | isJust $ winner field = []
  | otherwise             = fmap (setField field p) $ freePositions field where
    p = player field

-- | The current player.
player :: TicTacToe -> Player
player (TicTacToe x o) 
  | (countOnes $ x .|. o) `mod` 2 == 0 = PlayerX
  | otherwise                          = PlayerO

-- | All free positions on the field.
freePositions :: TicTacToe -> [Pos]
freePositions (TicTacToe x o) = filter (testBit bits . posToBit) positions where
  bits = complement $ x .|. o

-- | The winner of the field, if any.
winner :: TicTacToe -> Maybe Player
winner (TicTacToe x o)
  | hasWonBits x = Just PlayerX
  | hasWonBits o = Just PlayerO
  | otherwise    = Nothing

-- | All positions on the field.
positions :: [Pos]
positions = [(x, y) | y <- [0..2], x <- [0..2]]

gameInfo :: TicTacToe -> GameInfo
gameInfo field = case winner field of
  Just x  -> WonBy x
  Nothing -> if null (freePositions field) then Draw else InProgress

switch :: Player -> Player
switch PlayerX = PlayerO
switch PlayerO = PlayerX

--------------------------------------------------------------------------------
-- ** Internal operations

-- | Checks whether the player with that bitset has won.
hasWonBits :: Bits -> Bool
hasWonBits bits = not $ null $ filter (\b -> b == bits .&. b) winningBits

-- | Converts a position to the offset in the bitsets.
posToBit :: Pos -> Bits
posToBit (x, y) = x + (y * 3)

-- | Bit 
winningBits :: [Bits]
winningBits = [ 7, 56, 448, 73, 146, 292, 273, 84 ]

countOnes :: Bits -> Int
countOnes 0 = 0
countOnes x = countOnes (x `shiftR` 1) + if testBit x 0 then 1 else 0