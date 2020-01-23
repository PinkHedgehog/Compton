module Plant where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Word (Word8)
import System.Random
import Control.Monad (when)
--import Grap
type Radius = Float
type Theta = Float


data Plant = Plant { source   :: Point
                   , target   :: Point
                   , counter  :: (Radius, Theta)
                   , lambda   :: Float
                   , flag     :: Float
                   , lPressed :: Bool
                   , speed    :: Float
                   , lCompton :: Float
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

initPlant = Plant (-128, 0) (8, 12) (120, 30) 500 0 False 1 30

renderPlant :: Plant -> Picture
renderPlant plant = mappend txt $ scale 2.8 2.8 $ pictures [p1, p2, p3, p4, beam1, beam2, beam3]
    where (x, y) = source plant
          (i, o) = target plant
          (r, t) = counter plant
          lbd = lambda plant
          lc = if lPressed plant then 2.4e-3 else lCompton plant
          p0 = punktline [(-120, 0), (128, 0)]
          p1 = translate x y $ rectangleWire 16 16
          p2 = pictures [circleSolid i, circle o]
          p3 = arc t1 t2 r
          (t1, t2) = if t < 180 then (0, t) else (t, 360)
          phi = t / 180 * pi
          p4 = translate (r * cos phi) (r * sin phi) $! rotate t $ rectangleWire 16 16
          beam1 = color (colorWL lbd) $ translate (-64) 0 $ rectangleSolid 112 3
          beam2 = color (colorWL lbd') $ rotate (-t) $ translate 64 0 $ rectangleSolid 112 3
          beam3 = scale 1.0 3.0 $ color (colorWL lbd) $ punktline [(-8,0), (r,0)]
          lbd' = lbd + lc*(1 - cos phi)
          txt = pictures $! [ translate (-630) (-270) $ scale 0.2 0.2 $ Text $ "THETA = " ++ show (round t)
                            , translate (-630) (-300) $ scale 0.2 0.2 $ Text $ "LAMBDA = " ++ show (round lbd)
                            , translate (-630) (-360) $ scale 0.2 0.2 $ Text $ "LAMBDA_C = " ++ show lc
                            , translate (-630) (-330) $ scale 0.2 0.2 $ Text $ "LAMBDA' = " ++ show lbd'
                            ]



genAngle :: IO Int
genAngle = getStdRandom (randomR (0, 359))

updatePlantTime :: Float -> Plant -> IO Plant
updatePlantTime t plant = do
    let fl = flag plant
        (r, _) = counter plant
        sp = speed plant
    if fl >= 10
    then do
        angle <- fmap fromIntegral genAngle
        return $ plant {flag = 0, counter = (r, angle)}
    else return $ plant {flag = fl + sp}

updatePlantEvent event plant = 
    case event of
        EventKey (Char l) Down _ _ -> do
            let lp = lPressed plant
            return plant {lPressed = not lp}
        EventKey (SpecialKey KeyUp) Down (Modifiers _ Down _) _ -> do
            let lc = lCompton plant
                lbd = lambda plant
            return $! plant {lCompton = min ((700 - lbd)/2) (lc + 5)}
        EventKey (SpecialKey KeyDown) Down (Modifiers _ Down _) _ -> do
            let lc = lCompton plant
                lbd = lambda plant
            return $! plant {lCompton = max 0 (lc - 5)}
        EventKey (SpecialKey KeyUp) Down _ _ -> do
            let lbd = lambda plant
                lc  = lCompton plant
                lp  = lPressed plant
            return $! plant { lambda = if lp then 2.4e-3 else 
                if lbd + max 10 (2*lc) <= 700 then lbd + 10 else lbd}
        EventKey (SpecialKey KeyDown) Down _ _ -> do
            let lbd = lambda plant
            return $! plant {lambda = if (lPressed plant) then 2.4e-3 else max 380 (lbd - 10)}
        EventKey (SpecialKey KeyLeft) Down _ _ -> do
            let spd = speed plant
            return $! plant {speed = max 0.1 (spd-0.1)}
        EventKey (SpecialKey KeyRight) Down _ _ -> do
            let spd = speed plant
            return $! plant {speed = spd + 0.1}
        _ -> return plant


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
                                               b = 0
                                               g = ((645-lbd)/(645-580))**gamma
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

