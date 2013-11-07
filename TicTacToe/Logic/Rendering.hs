module Logic.Rendering
  ( emptyBoardPicture
  , drawBoard
  ) where

import Prelude

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Colour.SRGB

import Logic.TicTacToe

emptyBoardPicture :: QDiagram SVG R2 [(Int,Int)]
emptyBoardPicture = drawBoard initialField

drawBoard :: TicTacToe -> QDiagram SVG R2 [(Int,Int)]
drawBoard field = concatRows # scale 3 # alignTL
  where
    concatRows  = foldl1 (beside $ r2 (0,-30)) concatCols
    concatCols = map (foldl1 (beside $ r2 (30,0))) allCells
    allCells   = map rowCells [0..2]
    rowCells y = map (\p -> drawCell p (field `getField` p)) [(x,y) | x <- [0..2]]

drawCell :: Pos -> Maybe Player -> QDiagram SVG R2 [(Int,Int)]
drawCell pos Nothing  = cellBounds pos
drawCell pos (Just p) = drawPlayer p `atop` cellBounds pos

cellBounds :: Pos -> QDiagram SVG R2 [(Int,Int)]
cellBounds pos = square 30 # lw 3 # value [pos]

drawPlayer :: Player -> QDiagram SVG R2 [(Int,Int)]
drawPlayer PlayerX = (p2 (-13,-13) ~~ p2 (13,13) `atop` p2 (-13,13) ~~ p2 (13,-13)) 
                    # lw symThickness # lc xcolor # value []
drawPlayer PlayerO = circle 13 # lw symThickness # lc ocolor # value []

xcolor, ocolor :: Colour Double
xcolor = sRGB 0.6 0 0
ocolor = sRGB 0.208 0.376 0.58
symThickness :: Double
symThickness = 4