{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

square, wall, ground, storage, box :: Picture
square = solidRectangle 1 1
wall = colored black square
ground = colored (RGB 0.5 0.4 0.3) square
storage = colored red (solidCircle 0.13) & ground
box  = (cross 0.3 0.3) & (cross (-0.3) 0.3) & colored brown square

cross :: Double -> Double -> Picture
cross x y = colored black (polyline([(-x, -y), (x, y)]))

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank
  
maze :: Maze
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

boxes :: [Coord]
boxes = [(C x y) | x <- [0,1], y <- [0, 1]]

pictureOfMaze :: Picture
pictureOfMaze = pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (addBoxes boxes (removeBoxes maze) (C x y))) | x<-[-10..10], y<-[-10..10]])

data Direction = R | U | L | D

data Coord = C Integer Integer deriving (Eq)

data State = Coord Direction [Coord]

type Maze = Coord -> Tile

initialBoxes :: Maze -> [Coord]
initialBoxes func = [(C x y) | x <- [-10..10], y <- [-10..10], (func (C x y)) == Box]

removeBoxes :: Maze -> Maze
removeBoxes func = func'
  where func' coord | (func coord) == Box = Ground
        func' coord = (func coord)
        
addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes func = func'
  where func' coord | (elem coord boxes) == True = Box
        func' coord = func coord

main :: IO ()
main = drawingOf pictureOfMaze
