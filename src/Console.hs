module Console where

import Data.Map
import System.Console.ANSI
import GameMap

drawStartX = 1
drawStartY = 1

gameMapToList :: GameCellMap -> [(Point, Cell)]
gameMapToList gameCellMap = [makePair (x, y) | x <- [0..(cols-1)], y <- [0..(rows-1)]]
    where GameMap m (cols, rows) = gameCellMap
          makePair p = (p, getCell $ Data.Map.lookup p m)
          getCell (Just cell) = cell
          getCell _ = Space

drawPoint :: (Point, Cell) -> IO ()
drawPoint ((x, y), cellType) = do
    setCursorPosition (x+drawStartX+1) (y+drawStartY+1)
    putChar $ showANSI cellType

drawBorder :: Char -> Point -> IO ()
drawBorder c (x, y) = do
    setCursorPosition x y
    putChar c

drawGameCellMap :: GameCellMap -> IO ()
drawGameCellMap gm = 
    let GameMap m (cols, rows) = gm
        x0 = drawStartX
        y0 = drawStartY
        x1 = x0+cols+1
        y1 = y0+rows+1
        drawBorder2 c p1 p2 = do drawBorder c p1 ; drawBorder c p2
    in do
        mapM_ (\x_ -> drawBorder2 '|' (x_, y0) (x_, y1)) [x0..x1]
        mapM_ (\y_ -> drawBorder2 '-' (x0, y_) (x1, y_)) [y0..y1]
        mapM_ (drawBorder '+') [(x0,y0), (x1, y0), (x0, y1), (x1, y1)]
        mapM_ drawPoint $ gameMapToList gm
        putStrLn ""