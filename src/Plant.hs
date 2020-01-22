module Plant where

import Graphics.Gloss
import System.Random
import Control.Monad (when)
--import Grap
type Rad = Float
type Theta = Float

data Plant = Plant { source :: Point
                   , target :: Point
                   , counter :: (Rad, Theta)
                   , lambda :: Float
                   } deriving (Show, Eq)

--display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--
size = (1366, 768)

offset = (128, 128)

punktline :: [Point] -> Picture
punktline [(x1, y1), (x2, y2)] = pictures $ zipWith (\x y -> line [(x, y), (x+dx, y+dy)]) [x1,x1+2*dx..x2] [y1,y1+2*dy..y2]
    where dx = if x1 == x2 then 0 else (x2 - x1) / abs (x2 - x1)
          dy = if y1 == y2 then 0 else (y2 - y1) / abs (y2 - y1)


canvas = (InWindow "Compton's plant" size offset)

initPlant = Plant (-128, 0) (8, 12) (120, 30) 645

renderPlant :: Plant -> Picture
renderPlant (Plant (x, y) (i, o) (r, t) lbd) = scale 3.0 3.0 $ pictures [p1, p2, p3, p4, beam1, beam2, beam3]
    where p0 = punktline [(-120, 0), (128, 0)]
          p1 = translate x y $ rectangleWire 16 16
          p2 = pictures [circleSolid i, circle o]
          p3 = arc 0 t r
          phi = t / 180 * pi
          p4 = translate (r * cos phi) (r * sin phi) $! rotate t $ rectangleWire 16 16
          beam1 = color (colorWL lbd) $ translate (-64) 0 $ rectangleSolid 112 3
          beam2 = color (colorWL lbd') $ rotate (-t) $ translate 64 0 $ rectangleSolid 112 3
          beam3 = scale 1.0 3.0 $ color (colorWL lbd) $ punktline [(-8,0), (r,0)]
          lbd' = lbd - lc*(1 - cos phi)

lc = 132 --2.4e-3
{-
 -rollDice :: IO Int
 rollDice = getStdRandom (randomR (1,6))
 - -}

genAngle :: IO Int
genAngle = getStdRandom (randomR (1 :: Int, 359 :: Int))

updatePlantTime :: Float -> Plant -> IO Plant
updatePlantTime t p@(Plant s tr (r, theta) l) = do
    if (round t `mod` 6 == 0)
    then do
        angle <- fmap fromIntegral genAngle
        return $ Plant s tr (r, angle) l
    else return p

updatePlantEvent _ plant = return plant


colorWL :: Float -> Color
colorWL lbd = makeColor r g b 1
    where gamma = 0.8
          (r, g, b) = convert lbd
          convert lbd
              | lbd >= 380 && lbd <= 440 = let att = 0.3 + 0.7 * (lbd - 380) / (440 - 380)
                                               r   = (((440-lbd) / (440 - 380)) * att) ** gamma
                                               g   = 0
                                               b   = (1.0 * att) ** gamma
                                               in (r, g, b)
              | lbd >= 440 && lbd <= 490 = let r = 0
                                               g = ((lbd - 440)/(490-440)) ** gamma
                                               b = 1.0
                                               in (r, g, b)
              | lbd >= 490 && lbd <= 510 = let r = 0
                                               g = 1
                                               b = ((510 - lbd)/(510-490)) ** gamma
                                               in (r, g, b)
              | lbd >= 510 && lbd <= 580 = let r = ((lbd-510)/(580-510))**gamma
                                               g = 1
                                               b = 0
                                               in (r, g, b)
              | lbd >= 580 && lbd <= 645 = let r = 1.0
                                               g = 0
                                               b = ((645-lbd)/(645-580))**gamma
                                               in (r, g, b)
              | lbd >= 645 && lbd <= 750 = let att = 0.3 + 0.7*(750-lbd)/(750-645)
                                               r = (1.0*att) ** gamma
                                               g = 0
                                               b = 0
                                               in (r, g, b)
              | otherwise = (1, 1, 1)

colorWL2 lbd = 3.74183e-16 * wlm ** (-5.0) / ( exp (1.4388e-2 / (wlm * bbTemp)) - 1.0)
    where wlm = lbd * 1e-5
          bbTemp = 5000

