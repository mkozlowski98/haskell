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

player1 :: Picture
player1 = (colored yellow (solidPolygon([(0, -0.4), ((0.4*(sqrt 3)/2), 0.4/2), ((-0.4*(sqrt 3)/2), 0.4/2)])))

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank
  
maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
pictureOfMaze :: Picture
pictureOfMaze = pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y)) | x<-[-10..10], y<-[-10..10]])

data Direction = R | U | L | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 3 3

checkCoord :: Coord -> Bool
checkCoord (C x y)
  | maze x y == Storage || maze x y == Ground = True
  | otherwise                                 = False

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord D (C x y) = C x (y-1)

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) (C x y)
  | key == "Right" && checkCoord (C (x+1) y) = adjacentCoord R (C x y)
  | key == "Left"  && checkCoord (C (x-1) y) = adjacentCoord L (C x y)
  | key == "Up"    && checkCoord (C x (y+1)) = adjacentCoord U (C x y)
  | key == "Down"  && checkCoord (C x (y-1)) = adjacentCoord D (C x y)
handleEvent _ c    = c

drawState :: Coord -> Picture
drawState c = (atCoord c player1) & pictureOfMaze

walk1 :: IO ()
walk1 = activityOf initialCoord handleEvent drawState

main :: IO ()
main = walk1
