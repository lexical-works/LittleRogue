import GameMapGenerator (generateGameMap)
import Console (drawGameCellMap)
import System.Console.ANSI (clearScreen)

main :: IO ()
main = do
    gameMap <- generateGameMap (5, 5)
    clearScreen
    drawGameCellMap gameMap