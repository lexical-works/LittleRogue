import System.IO
import System.Console.ANSI
import Data.Map
import GameMap
import Console

genGameMap :: Int -> Int -> [(Int, Int)] -> GameCellMap
genGameMap cols rows wallList = GameMap (fromList list) (cols, rows)
    where list = [makeCell (x, y) | x <- [0..(cols-1)], y <- [0..(rows-1)]]
          makeCellType p = if p `elem` wallList then Wall else Floor
          makeCell p = (p, makeCellType p)

_gm = genGameMap 10 10 [(3,3), (3,5), (5,6)]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    clearScreen
    drawGameCellMap _gm
    keyChar <- getChar
    putChar keyChar