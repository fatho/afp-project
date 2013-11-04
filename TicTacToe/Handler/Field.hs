module Handler.Field (getFieldR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Logic.TicTacToe
import Logic.Rendering

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getFieldR :: Board -> Handler TypedContent
getFieldR board = do
  let pic = drawBoard board
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) pic
