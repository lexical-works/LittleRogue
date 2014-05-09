module GameMapGenerator (generateGameMap) where

import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map, empty, insert, unionWith)
import System.Random
import GameMap


type Room = Point
data Connection = Connection { startRoom :: Room 
                             , endRoom :: Room }
                             deriving (Show)


roomWidth :: Int
roomWidth = 7

roomHeight :: Int
roomHeight = 7

hallwayLength :: Int
hallwayLength = 2

roomSpaceWidth :: Int
roomSpaceWidth = roomWidth + hallwayLength

roomSpaceHeight :: Int
roomSpaceHeight = roomHeight + hallwayLength

roomDoorX :: Int
roomDoorX = roomWidth `div` 2

roomDoorY :: Int
roomDoorY = roomHeight `div` 2

roomDensity :: Float
roomDensity = 0.5


generateGameMap :: Size -> IO GameCellMap
generateGameMap size@(x, y) = do
    roomCandidates <- makeRooms size
    let connections = makeConnections roomCandidates
        rooms = removeIsolateRooms connections roomCandidates
        roomTiles = generateRoomTiles rooms
        connectionTiles = generateConnectionTiles connections
        tiles = connectionTiles `cellUnion` roomTiles
        mapSize = (roomSpaceWidth * x - hallwayLength, roomSpaceHeight * y - hallwayLength)
    return $ GameMap tiles mapSize

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

removeIsolateRooms :: [Connection] -> [Room] -> [Room]
removeIsolateRooms connections = filter isNotIsolate
    where isNotIsolate room = any (hasConnection room) connections
          hasConnection room (Connection start end) = room == start || room == end

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

generateRoomTiles :: [Room] -> Map Point Cell
generateRoomTiles = foldl generateRoomTile empty

generateRoomTile :: Map Point Cell -> Room -> Map Point Cell
generateRoomTile cells (roomX, roomY) = cells `cellUnion` floorCells `cellUnion` wallCells
    where minX = roomX * roomSpaceWidth
          maxX = (roomX + 1) * roomSpaceWidth - hallwayLength - 1
          minY = roomY * roomSpaceHeight
          maxY = (roomY + 1) * roomSpaceHeight - hallwayLength - 1
          rangeX = [minX .. maxX]
          rangeY = [minY .. maxY]
          wallPoints = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
          floorPoints = [(x, y) | x <- rangeX, y <- rangeY, x > minX && x < maxX, y > minY && y < maxY]
          wallCells = foldl (insertCell Wall) empty wallPoints
          floorCells = foldl (insertCell Floor) empty floorPoints

generateConnectionTiles :: [Connection] -> Map Point Cell
generateConnectionTiles = foldl generateConnectionTile empty

generateHorizontalConnectionTile :: Map Point Cell -> Connection -> Map Point Cell
generateHorizontalConnectionTile cells (Connection (startX, startY) (endX, endY)) = cells `cellUnion` floorCells `cellUnion` wallCells
    where left = startX * roomSpaceWidth + roomWidth - 1
          right = endX * roomSpaceWidth
          doorY = startY * roomSpaceHeight + roomDoorY
          wallTop = doorY - 1
          wallBottom = doorY + 1
          wallPoints = [(x, y) | x <- [left .. right], y <- [wallTop .. wallBottom]]
          floorPoints = [(x, doorY) | x <- [left .. right]]
          wallCells = foldl (insertCell Wall) empty wallPoints
          floorCells = foldl (insertCell Floor) empty floorPoints

generateVerticalConnectionTile :: Map Point Cell -> Connection -> Map Point Cell
generateVerticalConnectionTile cells (Connection (startX, startY) (endX, endY)) = cells `cellUnion` floorCells `cellUnion` wallCells
    where top = startY * roomSpaceHeight + roomHeight - 1
          bottom = endY * roomSpaceHeight
          doorX = startX * roomSpaceWidth + roomDoorX
          wallLeft = doorX - 1
          wallRight = doorX + 1
          wallPoints = [(x, y) | x <- [wallLeft .. wallRight], y <- [top .. bottom]]
          floorPoints = [(doorX, y) | y <- [top .. bottom]]
          wallCells = foldl (insertCell Wall) empty wallPoints
          floorCells = foldl (insertCell Floor) empty floorPoints

insertCell :: Cell -> Map Point Cell -> Point -> Map Point Cell
insertCell cell cells position = insert position cell cells

cellUnion :: Map Point Cell -> Map Point Cell -> Map Point Cell
cellUnion = unionWith unionCell
    where unionCell left right = if left == Floor || right == Floor then Floor else left

generateConnectionTile :: Map Point Cell -> Connection -> Map Point Cell
generateConnectionTile cells connection@(Connection (startX, startY) (endX, endY))
    | startY == endY = generateHorizontalConnectionTile cells connection
    | startX == endX = generateVerticalConnectionTile cells connection