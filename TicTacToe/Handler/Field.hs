module Handler.Field (getFieldR, drawBoard, emptyBoardPicture) where

import Import

import Data.Array
import Data.Text (unpack)

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Logic.TicTacToe

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getFieldR :: Board -> Handler TypedContent
getFieldR board = do
  let pic = drawBoard board
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) pic

emptyBoardPicture :: QDiagram SVG R2 [(Int,Int)]
emptyBoardPicture = drawBoard emptyBoard

drawBoard :: Board -> QDiagram SVG R2 [(Int,Int)]
drawBoard field = concatRows # scale 3 # alignTL
  where
    concatRows  = foldl1 (beside $ r2 (0,-30)) concatCols
    concatCols = map (foldl1 (beside $ r2 (30,0))) allCells
    allCells   = map rowCells [1..3]
    rowCells r = map (\p -> drawCell p (field `boardAt` p)) [(r,c) | c <- [1..3]]

drawCell :: Pos -> Cell -> QDiagram SVG R2 [(Int,Int)]
drawCell pos Nothing  = cellBounds pos
drawCell pos (Just p) = drawPlayer p `atop` cellBounds pos

cellBounds :: Pos -> QDiagram SVG R2 [(Int,Int)]
cellBounds pos = square 30 # lw 3 # value [pos] # showOrigin

drawPlayer :: Player -> QDiagram SVG R2 [(Int,Int)]
drawPlayer PlayerX = (p2 (-15,-15) ~~ p2 (15,15) `atop` p2 (-15,15) ~~ p2 (15,-15)) # lw 3 # value [] # showOrigin
drawPlayer PlayerO = circle 15 # lw 3 # value []
