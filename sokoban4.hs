import CodeWorld

elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False
elemList y (x:xs) = if y == x then True
                    else elemList y xs

appendList :: [a] -> [a] -> [a]
appendList xs [] = xs
appendList [] ys = ys
appendList (x:xs) (y:ys) = x : y : appendList xs ys

listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs) = if f x == True then x : filterList f xs
                      else filterList f xs

nth :: [a] -> Integer -> a
nth (x:xs) n = if n == 1 then x
               else nth xs (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = if x == False then False
                 else andList xs

allList :: (a -> Bool) -> [a] -> Bool
allList f [] = True
allList f (x:xs) = if f x == False then False
                   else allList f xs

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f y [] = y
foldList f y (x:xs) = f x (foldList f y xs)

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

bfsGraph :: Eq a => [a] -> [a] -> (a -> [a]) -> [a]
bfsGraph seen [] _ = seen
bfsGraph seen queue neighbours = if elemList first seen == True then bfsGraph seen (tail queue) neighbours
                                 else bfsGraph (first : seen) (appendList (tail queue) (neighbours first)) neighbours
                                 where first = nth queue 1

bfsWithCheck :: Eq a => [a] -> [a] -> (a -> [a]) -> (a -> Bool) -> Bool
bfsWithCheck seen [] _ _ = True
bfsWithCheck seen queue neighbours isOk = if isOk first == False then False
                                          else
                                            if elemList first seen == True then bfsWithCheck seen (tail queue) neighbours isOk
                                            else bfsWithCheck (first : seen) (appendList (tail queue) (neighbours first)) neighbours isOk
                                          where first = nth queue 1

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = bfsWithCheck (initial : []) (neighbours initial) neighbours isOk

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v (bfsGraph (initial : []) (neighbours initial) neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = andList (mapList isReachable vs) where isReachable x = reachable x initial neighbours

isClosed :: Maze -> Bool
isClosed (Maze coord drawMaze) = if drawMaze coord == Storage || drawMaze coord == Ground then isGraphClosed coord neighbours checkIfBlank
                                 else False
                                 where neighbours (C x y) = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                       checkIfWall coord = drawMaze coord /= Wall
                                       checkIfBlank coord = drawMaze coord /= Blank
  
isSane :: Maze -> Bool
isSane (Maze coord drawMaze) = listLength (filterList checkIfStorage reachableInMaze) >= listLength (filterList checkIfBox reachableInMaze)
                               where reachableInMaze = bfsGraph (coord : []) (neighbours coord) neighbours
                                     neighbours (C x y) = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                     checkIfWall coord = drawMaze coord /= Wall
                                     checkIfStorage coord = drawMaze coord == Storage
                                     checkIfBox coord = drawMaze coord == Box

checkMaze :: Maze -> Bool
checkMaze maze = isClosed maze && isSane maze

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

etap4 :: Picture
etap4 = pictureOfBools (appendList (mapList checkMaze mazes) (mapList checkMaze badMazes))

main :: IO()
main = drawingOf etap4

mazes :: [Maze]
mazes = [mazeGood1, mazeGood2]
badMazes :: [Maze]
badMazes = [mazeBad1, mazeBad2]

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
  | (x >= -1 && x <= 0) && y == 2              = Box
  | x == -1 && y == -1                         = Box
  | x == 1 && ((y >= 1 && y <= 3) || y == -2)  = Storage
  | otherwise                                  = Ground

mazeGood2 :: Maze
mazeGood2 = Maze (C 3 3) drawMazeGood2
drawMazeGood2 :: Coord -> Tile
drawMazeGood2 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

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

mazeBad2 :: Maze
mazeBad2 = Maze (C 0 0) drawMazeBad2
drawMazeBad2 :: Coord -> Tile
drawMazeBad2 (C x y)
  | abs x > 4 || abs y > 4   = Blank
  | x == 4 && y == 0         = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == -1 && y < 2         = Wall
  | y == 1 && x < -1         = Wall
  | x == -3                  = Storage
  | x == 2 && y < 0          = Box
  | otherwise                = Ground
