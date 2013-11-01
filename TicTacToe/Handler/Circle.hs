module Handler.Circle (getCircleR, picture) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getCircleR :: Handler TypedContent
getCircleR = return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) picture

picture :: QDiagram SVG R2 [String]
picture = (circle 20 # lw 1 # value ["the circle"] `atop` (translateX 10 (square 30 # lw 1 # value ["the square"]))) # scale 2 # pad 1.1 # alignTL
