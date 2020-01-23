module Main where

import Lib
import Plant
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO canvas white 30 initPlant (return . renderPlant) updatePlantEvent updatePlantTime
--display canvas white (renderPlant initPlant)
