module Console (
    drawGameCellMap
    ) where

import Data.Map (lookup)
import System.Console.ANSI
import GameMap

drawStartX = 1
drawStartY = 1

mul :: [a] -> [b] -> [(a, b)]
mul xs ys = [(x, y) | x <- xs, y <- ys]

gameMapToList :: GameCellMap -> [(Point, Maybe Cell)]
gameMapToList gm@(GameMap md sz@(cols, rows)) = map (\p -> (p, Data.Map.lookup p md)) $ mul [1..cols-1] [1..rows-1]

drawChar :: Char -> Point -> IO ()
drawChar c (x, y) = do
    setCursorPosition y x
    putChar c

drawCell :: (Point, Maybe Cell) -> IO ()
drawCell ((x, y), Just c) = drawChar (showANSI c) (x+drawStartX, y+drawStartY)
drawCell (p, Nothing) = drawChar ' ' p

drawGameCellMap :: GameCellMap -> IO ()
drawGameCellMap gm = 
    let GameMap m (cols, rows) = gm
        x0 = drawStartX
        y0 = drawStartY
        x1 = x0+cols
        y1 = y0+rows
        drawBorder2 c p1 p2 = do drawChar c p1 ; drawChar c p2
    in do
        mapM_ (\x_ -> drawBorder2 '-' (x_, y0) (x_, y1)) [x0..x1]
        mapM_ (\y_ -> drawBorder2 '|' (x0, y_) (x1, y_)) [y0..y1]
        mapM_ (drawChar '+') [(x0,y0), (x1, y0), (x0, y1), (x1, y1)]
        mapM_ drawCell $ gameMapToList gm
        setCursorPosition (y1+1) 0
