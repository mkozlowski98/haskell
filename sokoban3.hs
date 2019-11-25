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

player2 :: Direction -> Picture
player2 R = rotated (pi/2) player1
player2 L = rotated (3*pi/2) player1
player2 U = rotated pi player1
player2 D = player1

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

data Coord = C Integer Integer deriving (Eq)

initialCoord :: Coord
initialCoord = C 3 3

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

checkIfAllowed :: Coord -> [Coord] -> Bool
checkIfAllowed coord boxes
  | (addBoxes boxes (removeBoxes maze)) coord == Storage || (addBoxes boxes (removeBoxes maze)) coord == Ground = True
  | otherwise                                     = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord D (C x y) = C x (y-1)

checkCoord :: Coord -> Direction -> [Coord] -> Bool
checkCoord coord direction boxes
  | checkIfAllowed (adjacentCoord direction coord) boxes == True                                                = True
  | (addBoxes boxes (removeBoxes maze)) (adjacentCoord direction coord) == Box && checkIfAllowed (adjacentCoord direction (adjacentCoord direction coord)) boxes == True = True
  | otherwise                                                                                             = False

type Maze = Coord -> Tile

maze :: Maze
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

pictureOfMaze :: Maze -> Picture
pictureOfMaze func = pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (func (C x y))) | x<-[-10..10], y<-[-10..10]])

data Direction = R | U | L | D

initialDirection :: Direction
initialDirection = D

data State = S Coord Direction [Coord]

initialState :: State
initialState = S initialCoord initialDirection (initialBoxes maze)

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

adjacentBoxes :: [Coord] -> Coord -> Direction -> [Coord]
adjacentBoxes boxes coord direction = map (\x -> if (x == coord) then (adjacentCoord direction coord) else x) boxes

draw :: State -> Picture
draw (S coord direction boxes)
  | isWinning (S coord direction boxes) == True = scaled 3 3 (lettering "You won!")
  | otherwise = (atCoord coord (player2 direction)) & (pictureOfMaze (addBoxes boxes (removeBoxes maze)))

adjacentState :: State -> State
adjacentState (S coord direction boxes)
  | (addBoxes boxes (removeBoxes maze)) (adjacentCoord direction coord) == Box = (S (adjacentCoord direction coord) direction (adjacentBoxes boxes (adjacentCoord direction coord) direction))
  | otherwise                                 = (S (adjacentCoord direction coord) direction boxes)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S coord direction boxes)
  | isWinning (S coord direction boxes) == True = (S coord direction boxes)
  | key == "Right" && checkCoord coord R boxes = adjacentState (S coord R boxes)
  | key == "Left" && checkCoord coord L boxes  = adjacentState (S coord L boxes)
  | key == "Up" && checkCoord coord U boxes    = adjacentState (S coord U boxes)
  | key == "Down" && checkCoord coord D boxes  = adjacentState (S coord D boxes)
handleEvent _ state                      = state

allList :: [Bool] -> Bool
allList stmt = all (==True) stmt

isWinning :: State -> Bool
isWinning (S coord direction boxes) = allList (map (\x -> if (maze x == Storage) then True else False) boxes)

data Activity world = Activity world (Event -> world -> world) (world -> Picture)

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw) = Activity state0 handle' draw
    where handle' (KeyPress key) _ | key == "Esc" = state0
          handle' e s = handle e s

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw) = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    
simple :: Activity State
simple = Activity initialState handleEvent draw

runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw0) = activityOf state0 handle draw0

main :: IO ()
main = (runActivity (withStartScreen (resettable simple)))
