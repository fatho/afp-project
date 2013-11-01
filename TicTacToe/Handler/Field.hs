module Handler.Field (getFieldR, picture) where

import Import

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

getFieldR :: Handler TypedContent
getFieldR = return $ TypedContent typeSvg $ toContent $ renderSvg $ renderDia SVG (SVGOptions Absolute) picture

picture :: QDiagram SVG R2 [(Int, Int)]
picture = (foldl (beside (r2 (0,-30))) (row 1) [row 2, row 3]) # scale 3 # alignTL
	where row r = foldl (beside (r2 (30,0))) (cell 1) [cell 2, cell 3]
		where cell c = square 30 # lw 1 # value [(r, c)] # showOrigin


