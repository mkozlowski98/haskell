import CodeWorld

botCircle, topCircle, middleCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
topCircle c = colored c (translated 0   3  (solidCircle 1))
middleCircle c = colored c (translated 0 0 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Integer -> Picture
trafficLight 0  = botCircle green & middleCircle black & topCircle black & frame
trafficLight 1 = botCircle black & middleCircle yellow & topCircle black & frame
trafficLight 2 = botCircle black & middleCircle black & topCircle red & frame
trafficLight 3 = botCircle black & middleCircle yellow & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
   | round (t) `mod` 10 >= 0 && round (t) `mod` 10 <= 3  = trafficLight 0
   | round (t) `mod` 10 > 3 &&round (t) `mod` 10 <= 4 = trafficLight 1
   | round (t) `mod` 10 > 4 && round (t) `mod` 10 <= 8 = trafficLight 2
   | otherwise                = trafficLight 3
                                                  
main :: IO ()
main = animationOf trafficController
