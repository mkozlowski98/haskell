type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

wall, ground, storage, box :: Char
wall = '#'
box = '$'
storage = '.'
ground = ' '

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

main = putChar wall >> putChar storage
