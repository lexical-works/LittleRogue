module GameMap where

import Data.Map.Strict

type Point = (Int, Int)
data Cell = Floor | Wall deriving (Show, Eq)
data GameMap a = GameMap { mapData :: Map Point a
                         , mapSize :: Point
                         , playerPosition :: Point }
type GameCellMap = GameMap Cell