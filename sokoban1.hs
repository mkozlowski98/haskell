import CodeWorld

main :: Program
main = program
type Program = IO ()

program :: Program
program = drawingOf pictureOfMaze

square :: Picture
square = solidRectangle 1 1

wall :: Picture
wall = colored black square

ground :: Picture
ground = colored (RGB 0.5 0.4 0.3) square

storage :: Picture
storage = colored red (solidCircle 0.13) & ground

cross :: Double -> Double -> Picture
cross x y = colored black (polyline([(-x, -y), (x, y)]))

box :: Picture
box  = (cross 0.3 0.3) & (cross (-0.3) 0.3) & colored brown square

drawTile :: Integer -> Picture
drawTile n
  | n == 1    = wall
  | n == 2    = ground
  | n == 3    = storage
  | n == 4    = box
  | otherwise = blank
  
maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  
pictureOfMaze :: Picture
pictureOfMaze = pictures([translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y)) | x<-[-10..10], y<-[-10..10]])
