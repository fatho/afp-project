module Logic.MiniMax where

import Prelude
import Control.Arrow
import Data.List
import Data.Function

data Tree a = Node a [Tree a] deriving (Show)

data HistTree m a = HistNode a [(m,HistTree m a)] deriving (Show)

unfoldTree :: (a -> [a]) -> a -> Tree a
unfoldTree f x = Node x (map (unfoldTree f) $ f x)

unfoldHistTree :: (a -> [(m,a)]) -> a -> HistTree m a
unfoldHistTree f x = HistNode x (map (second (unfoldHistTree f)) (f x))

prune 0 (Node x _)  = Node x []
prune n (Node x ns) = Node x (map (prune $ n-1) ns) 

pruneHist 0 (HistNode x _)  = HistNode x []
pruneHist n (HistNode x ns) = HistNode x (map (second $ pruneHist $ n-1) ns)

maximize :: Ord a => Tree a -> a
maximize (Node x []) = x
maximize (Node x ns) = maximum $ fmap minimize ns

minimize :: Ord a => Tree a -> a
minimize (Node x []) = x
minimize (Node x ns) = minimum $ fmap maximize ns

maximizeHist :: Ord a => HistTree m a -> ([m],a)
maximizeHist (HistNode x []) = ([], x)
maximizeHist (HistNode x ns) = maximumBy (compare `on` snd) $ fmap (joinHist . second minimizeHist) ns

minimizeHist :: Ord a => HistTree m a -> ([m],a)
minimizeHist (HistNode x []) = ([], x)
minimizeHist (HistNode x ns) = minimumBy (compare `on` snd) $ fmap (joinHist . second maximizeHist) ns

joinHist :: (m, ([m],a)) -> ([m], a)
joinHist (m,(ms,x)) = (m:ms, x)