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

data DirectedCoord = DC Integer Integer Direction

initialDirectedCoord :: DirectedCoord
initialDirectedCoord = DC 3 3 D

adjacentDirectedCoord :: Direction -> DirectedCoord -> DirectedCoord
adjacentDirectedCoord R (DC x y d) = DC (x+1) y R
adjacentDirectedCoord L (DC x y d) = DC (x-1) y L
adjacentDirectedCoord U (DC x y d) = DC x (y+1) U
adjacentDirectedCoord D (DC x y d) = DC x (y-1) D

handleDirectedEvent :: Event -> DirectedCoord -> DirectedCoord
handleDirectedEvent (KeyPress key) (DC x y d)
  | key == "Right" && checkCoord (C (x+1) y) = adjacentDirectedCoord R (DC x y d)
  | key == "Left"  && checkCoord (C (x-1) y) = adjacentDirectedCoord L (DC x y d)
  | key == "Up"    && checkCoord (C x (y+1)) = adjacentDirectedCoord U (DC x y d)
  | key == "Down"  && checkCoord (C x (y-1)) = adjacentDirectedCoord D (DC x y d)
handleDirectedEvent _ dc    = dc

player2 :: Direction -> Picture
player2 R = rotated (pi/2) player1
player2 L = rotated (3*pi/2) player1
player2 U = rotated pi player1
player2 D = player1

drawDirectedState :: DirectedCoord -> Picture
drawDirectedState (DC x y d) = (atCoord (C x y) (player2 d)) & pictureOfMaze

walk2 :: IO ()
walk2 = activityOf initialDirectedCoord handleDirectedEvent drawDirectedState

{- Funkcja resettableActivityOf po naciśnięciu klawisza Escape zwraca stan początkowy (powraca do stanu początkowego),
   przy puszczeniu klawisza Escape stan gry pozostaje niezmieniony (przekazujemy to zdarzenie do handlera podanego w argumentach funkcji). -}
resettableActivityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resettableActivityOf initialWorld eventHandler draw = activityOf initialWorld resettableHandler draw
  where resettableHandler (KeyPress "Esc") c = initialWorld
        resettableHandler key c              = eventHandler key c
        
walk3 :: IO ()
walk3 = resettableActivityOf initialDirectedCoord handleDirectedEvent drawDirectedState 

main :: IO ()
main = walk3
