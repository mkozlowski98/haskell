import System.IO

mazes :: [Maze]
mazes = [Maze (C 0 3) maze1, Maze (C 3 3) maze2]
badMazes :: [Maze]
badMazes = [Maze (C (-1) (-2)) maze3, Maze (C 0 0) maze4]

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

maze2 :: Maze1
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

isClosed :: Maze -> Bool
isClosed (Maze coord drawMaze) = if drawMaze coord == Storage || drawMaze coord == Ground then isGraphClosed coord neighbours checkIfBlank
                                 else False
                                 where neighbours (C x y)
                                        | drawMaze (C x y) == Blank = []
                                        | otherwise = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                       checkIfWall coord = drawMaze coord /= Wall
                                       checkIfBlank coord = drawMaze coord /= Blank
  
isSane :: Maze -> Bool
isSane (Maze coord drawMaze) = listLength (filterList checkIfStorage reachableInMaze) >= listLength (filterList checkIfBox reachableInMaze)
                               where reachableInMaze = bfs [coord] (neighbours coord) neighbours
                                     neighbours (C x y) = filterList checkIfWall [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]
                                     checkIfWall coord = drawMaze coord /= Wall
                                     checkIfStorage coord = drawMaze coord == Storage
                                     checkIfBox coord = drawMaze coord == Box

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral (div k 2)) (fromIntegral k) (go 0 xs)
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

        pictureOfBool True = addCharAtInitialPos 'o'
        pictureOfBool False = addCharAtInitialPos 'x'

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

type Screen = String

data Tile = Wall | Ground | Storage | Box | Blank | BoxG deriving Eq

data Direction = R | L | U | D deriving Eq

data Coord = C Integer Integer deriving Eq

type Maze1 = Coord -> Tile

data Maze = Maze Coord (Coord -> Tile)

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
    
data Interaction world = Interaction world (Event -> world -> world) (world -> Screen)

data Event = KeyPress String

addCharAtInitialPos :: Char -> Picture
addCharAtInitialPos c d = (\x y -> if x == 0 && y == 0 then c else d x y)

wall, ground, storage, box, blank, player, playerG :: Picture
wall = addCharAtInitialPos '#'
ground = addCharAtInitialPos ' '
storage = addCharAtInitialPos '.'
box = addCharAtInitialPos '$'
player = addCharAtInitialPos '@'
playerG = addCharAtInitialPos '+'
blank = id

(&) = (.)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

translated :: Integer -> Integer -> Picture -> Picture
translated x y pic = \f x' y' -> pic (\x'' y'' -> f (x'' + x) (y'' + y)) (x' - x) (y' - y)

pictureOfMaze :: Maze -> Picture
pictureOfMaze (Maze c@(C x y) maze) = foldList (\(x, y) pic -> pic & (translated (x) (y) (drawTile (maze (C x y))))) blank (bfs [(x, y)] (neighbours (x, y)) neighbours)
  where
     neighbours (x, y)
       | maze (C x y) == Blank = []
       | otherwise = [((x-1), y), ((x+1), y), (x, (y-1)), (x, (y+1))]

bfs :: Eq a => [a] -> [a] -> (a -> [a]) -> [a]
bfs seen [] _ = seen
bfs seen queue neighbours = if elemList first seen then bfs seen (tail queue) neighbours
                            else bfs (first : seen) (appendList (tail queue) (neighbours first)) neighbours
                            where first = nth queue 0
                            
isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = andList (mapList isOk nodes)
                                        where nodes = bfs [initial] (neighbours initial) neighbours
                                        
reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v (bfs [initial] (neighbours initial) neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = andList (mapList isReachable vs) where isReachable x = reachable x initial neighbours

checkIfFree :: Coord -> Maze1 -> Bool
checkIfFree coord myMaze
  | myMaze coord == Storage || myMaze coord == Ground = True
  | otherwise                                         = False

checkIfBoxAllowed :: Direction -> Coord -> Maze1 -> Bool
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

initialBoxes :: Maze -> [Coord]
initialBoxes (Maze coord draw) = filterList (\x -> draw x == Box) (bfs [coord] (neighbours coord) neighbours)
                                 where
                                    neighbours c@(C x y)
                                      | draw c == Blank = []
                                      | otherwise = [(C (x-1) y), (C (x+1) y), (C x (y-1)), (C x (y+1))]

removeBoxes :: Maze1 -> Maze1
removeBoxes func = func'
  where func' coord | func coord == Box = Ground
        func' coord = func coord

addBoxes :: [Coord] -> Maze1 -> Maze1
addBoxes boxes func = func'
  where func' coord | elem coord boxes = Box
        func' coord = func coord

adjacentBoxes :: [Coord] -> Coord -> Direction -> [Coord]
adjacentBoxes boxes coord direction = Prelude.map (\x -> if (x == coord) then (adjacentCoord direction coord) else x) boxes

initialState :: Maze -> State
initialState maze@(Maze initialCoord drawMaze) = S initialCoord D (initialBoxes maze) 0 0

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state@(S coord direction boxes noMaze noMoves)
  | isWinning state && noMaze == listLength mazes - 1 = state
  | isWinning state = S newC D (initialBoxes (Maze newC newMaze)) (noMaze + 1) 0
  | key == "\ESC[C" || key == "\ESC[D" || key == "\ESC[A" || key == "\ESC[B" =
    S newCoord newDirection (adjacentBoxes boxes newCoord newDirection) noMaze (noMoves + 1)
  where
    newCoord = adjacentCoordIfAllowed coord newDirection actMaze
    newDirection
      | key == "\ESC[C" = R
      | key == "\ESC[D" = L
      | key == "\ESC[A" = U
      | key == "\ESC[B" = D
    actMaze = addBoxes boxes (removeBoxes currMaze)
    Maze initialCoord currMaze = nth mazes noMaze
    Maze newC newMaze = nth mazes (noMaze + 1)
handleEvent _ s = s

isWinning :: State -> Bool
isWinning (S coord direction boxes noMaze noMoves) = allList checkIfStorage boxes
  where checkIfStorage coord = maze coord == Storage
        Maze coord maze = nth mazes noMaze

drawState :: State -> Screen
drawState state@(S c@(C x y) direction boxes noMaze noMoves) =
  if isWinning state && noMaze < numberLevels - 1 then "Poziom ukończony, liczba ruchów: " ++ (show noMoves)
  else if isWinning state then "Poziom ukończony, liczba ruchów: " ++ (show noMoves) ++ "\n" ++ endScreen
  else [if x == 39 then '\n' else draw x y | y <- [13, 12..(-10)], x <- [-40..39]]
  where
    Maze coord maze = if noMaze < numberLevels then nth mazes noMaze else nth mazes 0
    numberLevels = listLength mazes
    playerPic = translated x y (if maze c == Storage then playerG else player)
    actMaze = (addBoxes boxes (removeBoxes maze))
    draw = (playerPic & pictureOfMaze (Maze coord actMaze)) (\x y -> ' ')

getSingleKey :: IO String
getSingleKey = reverse <$> getKey ""
    where getKey chars = do
            char <- getChar
            isNext <- hReady stdin
            (if isNext then getKey else return) (char:chars)
            

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state handle draw) = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    putStr "\ESCc"
    putStr (draw state)
    handleEvent state
    where
      handleEvent state' = do
        key <- getSingleKey
        let state'' = handle (KeyPress key) state'
        putStr "\ESCc"
        putStr (draw state'')
        handleEvent state''
    
resettable :: Interaction s -> Interaction s
resettable (Interaction state0 handle draw) = Interaction state0 handle' draw
    where handle' (KeyPress key) _ | key == "\ESC" = state0
          handle' e s = handle e s
          
data SSState world = StartScreen | Running world

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 handle draw) = Interaction state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = etap4
    draw' (Running s) = draw s

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle draw) = Interaction state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

etap4 :: Screen
etap4 = "SOKOBAN\n" ++ "Zamknięte poziomy dobre i złe" ++
        [if x == 10 then '\n' else draw x y | y <- [10, 9..(-10)], x <- [-10..10]]
        where draw = (translated 0 4 (pictureOfBools (mapList isClosed mazes)) & translated 4 4 (pictureOfBools (mapList isClosed badMazes))) (\x y -> ' ')

endScreen :: Screen
endScreen = "Wygrałeś! (Brak więcej poziomów)"


etap5 :: IO ()
etap5 = (runInteraction . withStartScreen . resettable . withUndo) (Interaction (initialState $ nth mazes 0) handleEvent drawState)

main :: IO ()
main = etap5

elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False
elemList y (x:xs) = y == x || elemList y xs

appendList :: [a] -> [a] -> [a]
appendList xs [] = xs
appendList [] ys = ys
appendList (x:xs) (y:ys) = x : y : appendList xs ys

listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs) = if f x then x : ys else ys
                      where ys = filterList f xs

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
allList f (x:xs) = if not (f x) then False
                   else allList f xs

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f y [] = y
foldList f y (x:xs) = f x (foldList f y xs)
