module GameMapGenerator where

import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import System.Random
import GameMap


type Room = Point
data Connection = Connection { startRoom :: Room 
                             , endRoom :: Room }
                             deriving (Show)


roomWidth :: Int
roomWidth = 5

roomHeight :: Int
roomHeight = 5

roomDensity :: Float
roomDensity = 0.5


generateGameMap :: Size -> IO GameCellMap
generateGameMap (width, height) = undefined

dropAt :: Int -> [a] -> [a]
dropAt index list = left ++ tail right
	where (left, right) = splitAt index list

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle list = do
	index <- randomRIO (0, length list - 1)
	let element = list !! index
	others <- shuffle $ dropAt index list
	return $ element : others

makeRooms :: Size -> IO [Room]
makeRooms (width, height) = do
	let allRooms = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]
	candidates <- shuffle allRooms
	let roomCount = floor $ fromIntegral width * fromIntegral height * roomDensity
	return $ take roomCount candidates

makeConnections :: [Room] -> [Connection]
makeConnections rooms = foldl (makeConnection rooms) [] rooms

makeConnection :: [Room] -> [Connection] -> Room -> [Connection]
makeConnection rooms connections room = connections ++ catMaybes newConnections
	where adjRooms = [findRightRoom room rooms, findBottomRoom room rooms]
	      newConnections = map (fmap $ Connection room) adjRooms

findRightRoom :: Room -> [Room] -> Maybe Room
findRightRoom room rooms = if null rightRooms then Nothing else Just (minimumBy compareX rightRooms)
	where rightRooms = [r | r <- rooms, snd r == snd room, fst r > fst room]
	      compareX a b = compare (fst a) (fst b)

findBottomRoom :: Room -> [Room] -> Maybe Room
findBottomRoom room rooms = if null bottomRooms then Nothing else Just (minimumBy compareY bottomRooms)
	where bottomRooms = [r | r <- rooms, fst r == fst room, snd r > snd room]
	      compareY a b = compare (snd a) (snd b)