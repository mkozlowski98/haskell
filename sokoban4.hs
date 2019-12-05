import CodeWorld

square, wall, ground, storage, box :: Picture
square = solidRectangle 1 1
wall = colored black square
ground = colored (RGB 0.5 0.4 0.3) square
storage = colored red (solidCircle 0.13) & ground
box  = (cross 0.3 0.3) & (cross (-0.3) 0.3) & colored brown square

player1 :: Picture
player1 = (colored yellow (solidPolygon([(0, -0.4), ((0.4*(sqrt 3)/2), 0.4/2), ((-0.4*(sqrt 3)/2), 0.4/2)])))

cross :: Double -> Double -> Picture
cross x y = colored black (polyline([(-x, -y), (x, y)]))

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

data Coord = C Integer Integer deriving (Eq)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Maze = Maze Coord (Coord -> Tile)
mazes :: [Maze]
mazes = [mazeGood1]
badMazes :: [Maze]
badMazes = [mazeBad1]

pictureOfMaze :: Maze -> Picture
pictureOfMaze (Maze coord draw) = (atCoord coord player1) & pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (draw (C x y))) | x<-[-10..10], y<-[-10..10]])

mazeGood1 :: Maze
mazeGood1 = Maze (C 0 3) drawMazeGood1
drawMazeGood1 :: Coord -> Tile
drawMazeGood1 (C x y)
  | abs x > 3 || y < -3 || y > 4               = Blank
  | abs x == 3 && (y < -1 || y > 2)            = Blank
  | abs x == 2 && (y < -2 || y > 3)            = Blank
  | (x >= -1 && x <= 1) && (y == -3 || y == 4) = Wall
  | abs x == 3 && (y >= -1 && y <= 2)          = Wall
  | abs x == 2 && (y == 3 || y == -2)          = Wall
  | x == 0 && abs y == 1                       = Wall
  | (x >= -1 && x <= 1) && y == 2              = Box
  | x == -1 && y == -1                         = Box
  | x == 1 && ((y >= 1 && y <= 3) || y == -2)  = Storage
  | otherwise                                  = Ground

mazeBad1 :: Maze
mazeBad1 = Maze (C (-1) (-2)) drawMazeBad1
drawMazeBad1 :: Coord -> Tile
drawMazeBad1 (C x y)
  | abs x > 3 || abs y > 3                  = Blank
  | abs x == 3 || abs y == 3                = Wall
  | abs x == 1 && abs y == 1                = Wall
  | x == 1 && y == 2 || x == 2 && y == 1    = Wall
  | x == 0 && (y == 2 || y == 0 || y == -2) = Box
  | x == 0 && y == -2                       = Storage
  | x == 2 && (y == 2 || y == -1)           = Storage
  | otherwise                               = Ground

main :: IO ()
main = drawingOf (pictureOfMaze mazeBad1)
