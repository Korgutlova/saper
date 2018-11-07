module Pics where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Types


data Game = Game 
            { board :: ExploredBoard
            , win :: Bool
            }

example = [ [Mine, Open 1, Open 2, Open 3], 
          [NotOpen, Open 3, Open 4, Mine],
          [Open 1, Open 2, Open 3, Mine]] 

initialWorld :: Game
initialWorld = Game
    { board = example 
    , win = False
    }

draw :: Game -> Picture
draw game = translate (w) (h) (scale c c (pictures [drawGrid]))
    where
    c = fromIntegral size
    w = fromIntegral width
    h = fromIntegral height

drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral width
    m = fromIntegral height

handleEvent :: Event -> Game -> Game
handleEvent _ w = w

handleTime :: Float -> Game -> Game
handleTime _ w = w

appMain = play  (InWindow "Saper Game" (screenWidth, screenHeight) (0, 0)) 
                (greyN 0.5) 
                100 
                initialWorld 
                draw 
                handleEvent 
                handleTime


-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = size * width

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = size * height 