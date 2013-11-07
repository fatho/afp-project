module Handler.Field (getFieldR) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Logic.TicTacToe
import Logic.Rendering

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getFieldR :: TicTacToe -> Handler TypedContent
getFieldR field = do
  let pic = drawBoard field
  return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) pic
