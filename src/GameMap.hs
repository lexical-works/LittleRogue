module GameMap where

import Data.Map.Strict

type Point = (Int, Int)
type Size = Point
data Cell = Floor | Wall
    deriving (Show, Eq)
data GameMap a = GameMap { mapData :: Map Point a
                         , mapSize :: Point
                         , playerPosition :: Point }
type GameCellMap = GameMap Cell

class ShowANSI a where
    showANSI :: a -> Char

instance ShowANSI Cell where
    showANSI Floor = ' '
    showANSI Wall = '#'
