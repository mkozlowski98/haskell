{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.Maybe
import Data.Text

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
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n-1)

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

player2 :: Direction -> Picture
player2 R = rotated (pi/2) player1
player2 L = rotated (3*pi/2) player1
player2 U = rotated pi player1
player2 D = player1

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

checkIfFree :: Coord -> (Coord -> Tile) -> Bool
checkIfFree coord myMaze
  | myMaze coord == Storage || myMaze coord == Ground = True
  | otherwise                                         = False

checkIfBoxAllowed :: Direction -> Coord -> (Coord -> Tile) -> Bool
checkIfBoxAllowed direction coord myMaze
  | myMaze coord == Box && checkIfFree (adjacentCoord direction coord) myMaze = True
  | otherwise                                                                 = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord D (C x y) = C x (y-1)

adjacentCoordIfAllowed :: Coord -> Direction -> (Coord -> Tile) -> Coord
adjacentCoordIfAllowed coord direction myMaze =
  if checkIfFree (adjacentCoord direction coord) myMaze || checkIfBoxAllowed direction (adjacentCoord direction coord) myMaze then adjacentCoord direction coord
  else coord

data Maze = Maze Coord (Coord -> Tile)

data Direction = R | U | L | D deriving (Eq)

initialDirection :: Direction
initialDirection = D

data State = S {
  coord :: Coord,
  direction :: Direction,
  boxes :: [Coord],
  noMaze :: Integer,
  noMoves :: Integer
}

instance Eq State where
  S coord direction boxes noMaze noMoves == S coord' direction' boxes' noMaze' noMoves' = coord == coord' &&
    direction == direction' && boxes == boxes' && noMaze == noMaze' && noMoves == noMoves'

initialState :: Maze -> State
initialState maze@(Maze coord drawMaze) = S coord initialDirection (initialBoxes maze) 0 0

initialBoxes :: Maze -> [Coord]
initialBoxes (Maze coord func) = filterList (\x -> if (func x == Box) then True else False) (bfsGraph (coord : []) (neighbours coord) neighbours)
                                 where neighbours (C x y) = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                       checkIfWall coord = func coord /= Wall

removeBoxes :: (Coord -> Tile) -> (Coord -> Tile)
removeBoxes func = func'
  where func' coord | (func coord) == Box = Ground
        func' coord = (func coord)
        
addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes boxes func = func'
  where func' coord | (elem coord boxes) == True = Box
        func' coord = func coord

adjacentBoxes :: [Coord] -> Coord -> Direction -> [Coord]
adjacentBoxes boxes coord direction = Prelude.map (\x -> if (x == coord) then (adjacentCoord direction coord) else x) boxes

draw :: State -> Picture
draw state@(S coord direction boxes noMaze noMoves) =
  if isWinning state && noMaze < numberLevels - 1 then lettering (append "Poziom ukończony, liczba ruchów: " (Data.Text.pack (show noMoves)))
  else if isWinning state && noMaze >= numberLevels - 1 then endScreen
  else (atCoord coord (player2 direction)) & (pictureOfMaze (addBoxes boxes (removeBoxes maze)))
  where numberLevels = listLength mazes
        Maze initialCoord maze = if noMaze < numberLevels then nth mazes noMaze else nth mazes 0

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state@(S coord direction boxes noMaze noMoves)
  | isWinning state && noMaze == (listLength mazes) - 1 = state
  | isWinning state = (S newC D (initialBoxes (Maze newC newMaze)) (noMaze+1) 0)
  | key == "Right" || key == "Left" || key == "Up" || key == "Down" =
      (S newCoord newDirection (adjacentBoxes boxes newCoord newDirection) noMaze (noMoves + 1))
  where
    newCoord = adjacentCoordIfAllowed coord newDirection actMaze
    newDirection
      | key == "Right" = R
      | key == "Left"  = L
      | key == "Up"    = U
      | key == "Down"  = D
    actMaze = addBoxes boxes (removeBoxes currMaze)
    Maze initialCoord currMaze = nth mazes noMaze
    Maze newC newMaze = nth mazes (noMaze + 1)
handleEvent _ s = s

isWinning :: State -> Bool
isWinning (S coord direction boxes noMaze noMoves) = allList checkIfStorage boxes
  where checkIfStorage coord = currMaze coord == Storage
        Maze coord currMaze = nth mazes noMaze

data Activity world = Activity world (Event -> world -> world) (world -> Picture)

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw) = Activity state0 handle' draw
    where handle' (KeyPress key) _ | key == "Esc" = state0
          handle' e s = handle e s

endScreen :: Picture
endScreen = scaled 3 3 (lettering "Wygrałeś!") & translated 0 (-2) (lettering "Naciśnij escape aby rozpocząć ponownie")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = etap4
    draw' (Running s) = draw s

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

simple :: Maze -> Activity State
simple maze = Activity (initialState maze) handleEvent draw

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw0) = activityOf state0 handle draw0

etap5 :: IO ()
etap5 = (runActivity . withStartScreen . resettable . withUndo) (simple (nth mazes 0))

main :: IO ()
main = etap5

bfsGraph :: Eq a => [a] -> [a] -> (a -> [a]) -> [a]
bfsGraph seen [] _ = seen
bfsGraph seen queue neighbours = if elemList first seen == True then bfsGraph seen (Prelude.tail queue) neighbours
                                 else bfsGraph (first : seen) (appendList (Prelude.tail queue) (neighbours first)) neighbours
                                 where first = nth queue 0

bfsWithCheck :: Eq a => [a] -> [a] -> (a -> [a]) -> (a -> Bool) -> Bool
bfsWithCheck seen [] _ _ = True
bfsWithCheck seen queue neighbours isOk = if isOk first == False then False
                                          else
                                            if elemList first seen == True then bfsWithCheck seen (Prelude.tail queue) neighbours isOk
                                            else bfsWithCheck (first : seen) (appendList (Prelude.tail queue) (neighbours first)) neighbours isOk
                                          where first = nth queue 0

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
                               where reachableInMaze = bfsGraph [coord] (neighbours coord) neighbours
                                     neighbours (C x y) = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                     checkIfWall coord = drawMaze coord /= Wall
                                     checkIfStorage coord = drawMaze coord == Storage
                                     checkIfBox coord = drawMaze coord == Box

checkMaze :: Maze -> Bool
checkMaze maze = isClosed maze && isSane maze

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = Prelude.length xs
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
etap4 = translated 0 6 (scaled 3 3 (lettering "Sokoban")) &
        translated 0 4 (lettering "Zamknięte poziomy dobre") &
        (pictureOfBools (mapList isClosed mazes)) &
        translated 0 0 (lettering "Zamknięte poziomy złe") &
        translated 0 (-4) (pictureOfBools (mapList isClosed badMazes)) &
        translated 0 (-4) (lettering "Rozsądne poziomy dobre") &
        translated 0 (-8) (pictureOfBools (mapList isSane mazes))
        -- translated 0 (-8) (lettering "Rozsądne poziomy złe") &
        -- translated 0 (-12) (pictureOfBools (mapList isSane badMazes))

mazes :: [Maze]
mazes = [Maze (C 0 3) maze1, Maze (C 3 3) maze2]
badMazes :: [Maze]
badMazes = [Maze (C (-1) (-2)) maze3, Maze (C 0 0) maze4]

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze draw = pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (draw (C x y))) | x<-[-10..10], y<-[-10..10]])

maze1 :: Coord -> Tile
maze1 (C x y)
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

maze2 :: Coord -> Tile
maze2 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze3 :: Coord -> Tile
maze3 (C x y)
  | abs x > 3 || abs y > 3                  = Blank
  | abs x == 3 || abs y == 3                = Wall
  | abs x == 1 && abs y == 1                = Wall
  | x == 1 && y == 2 || x == 2 && y == 1    = Wall
  | x == 0 && (y == 2 || y == 0 || y == -2) = Box
  | x == 0 && y == -2                       = Storage
  | x == 2 && (y == 2 || y == -1)           = Storage
  | otherwise                               = Ground

maze4 :: Coord -> Tile
maze4 (C x y)
  | abs x > 4 || abs y > 4   = Blank
  | x == 4 && y == 0         = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == -1 && y < 2         = Wall
  | y == 1 && x < -1         = Wall
  | x == -3                  = Storage
  | x == 2 && y < 0          = Box
  | otherwise                = Ground
