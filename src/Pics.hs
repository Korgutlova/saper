module Pics where

import Data.Foldable
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as G
import Graphics.Gloss.Juicy

import Types
import Debug.Trace


startGame :: IO()
startGame = do 
        world <- initialWorld
        play FullScreen (greyN 0.5) 100 world  draw handleEvent handleTime

data Game = Game 
            { board :: ExploredBoard
            , imgs  :: Images
            }

data Images = Images 
            { mina  :: Picture
            , flag  :: Picture 
            , block :: Picture
            , open  :: Picture
            }

loadImages :: IO Images
loadImages = Images
  <$> fmap fold (loadJuicyPNG "img/bomb.png")
  <*> fmap fold (loadJuicyPNG "img/flag.png")
  <*> fmap fold (loadJuicyPNG "img/block.png")
  <*> fmap fold (loadJuicyPNG "img/open.png")

example = [ [Cell 1, Cell 2, NotOpen, NotOpen, Cell 1, NotOpen, NotOpen, NotOpen, Cell 1, NotOpen] 
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, Mine, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, Cell 4, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, Cell 5, MineFlag, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          , [NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen, NotOpen]
          ]


initialWorld :: IO Game
initialWorld = createGame <$> loadImages

createGame :: Images -> Game
createGame images = Game
    { board = example 
    , imgs  = images
    }

draw :: Game -> Picture
draw game = pictures 
    [ drawEmpty x y s c (open (imgs game))  
    , drawGrid
    , drawBoard x y s txt c game]
    where
        c = fromIntegral size
        s = 0.096
        txt = 0.3
        x = fromIntegral initX
        y = fromIntegral initY

generateArray :: (Float, Float) -> Float -> Float  -> [(Float, Float)]
generateArray (0, y) k h = case k of 
                        (-1) -> (0, 0):[]
                        otherwise -> (0, y):generateArray (h, k) (k - 1) h
generateArray (x, y) k h = (x, y):generateArray (x-1, y) k h

drawGrid :: Picture
drawGrid = color (greyN 0.8) (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [0..m]
    vs = map (\i -> line [(i, 0), (i, m)]) [0..n]

    n = fromIntegral width
    m = fromIntegral height

drawEmpty :: Float -> Float -> Float -> Float -> Picture -> Picture
drawEmpty x y s c img = pictures (b)
    where
        b = map (\(i, j) -> translate (x + j * c) (y - i * c) (scale s s img)) (generateArray (n, m) m n)
        n = fromIntegral width - 1
        m = fromIntegral height - 1

drawBoard :: Float -> Float -> Float -> Float -> Float -> Game -> Picture
drawBoard x y s txt c game = pictures (b)
    where
        b = map (\(i, j) -> case (getElem cust_map i j) of 
                            NotOpen -> translate (x + j * c) (y - i * c) (scale s s (block (imgs game)))
                            Mine -> translate (x + j * c) (y - i * c) (scale s s (mina (imgs game)))
                            MineFlag -> translate (x + j * c) (y - i * c) (scale s s (flag (imgs game)))
                            Cell z -> translate (x + j * c - deltax) (y - i * c - deltay) 
                                        (scale txt txt (color black $ text (show z))))
            (generateArray (n, m) m n)
        cust_map = (board game)
        n = fromIntegral width - 1
        m = fromIntegral height - 1
        deltax = fromIntegral 10
        deltay = fromIntegral 15

getElem :: ExploredBoard -> Float -> Float -> CellState Int
getElem cust_map i j = cust_map !! (round i) !! (round j)

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) game = openCell (mouseToCell mouse) game
handleEvent (EventKey (MouseButton RightButton) Down _ mouse) game = setFlag (mouseToCell mouse) game
handleEvent _ w = w

handleTime :: Float -> Game -> Game
handleTime _ w = w

-- fix function
mouseToCell :: G.Point  -> (Int, Int)
mouseToCell (x, y) = (i, j)
        where   
            i = floor (x - fromIntegral initX) `div` size            
            j = floor (y + fromIntegral initY) `div` size

changeCell2 :: Types.Point -> [[a]] -> a -> [[a]]
changeCell2 (x, y) map newElem = 
    let modified_row = replace2 y (map !! x) newElem
    in replace2 x map modified_row

replace2 :: Int -> [a] -> a -> [a]
replace2 x map newElem = take x map ++ newElem : drop (x+1) map

openCell :: (Int, Int) -> Game -> Game
openCell (x, y) game = Game 
                    { board = changeCell2 (x, y) (board game) (Cell 5)
                    , imgs = (imgs game)
                    }
setFlag :: (Int, Int) -> Game -> Game
setFlag (x, y) game = Game 
                    { board = changeCell2 (x, y) (board game) (MineFlag)
                    , imgs = (imgs game)
                    }

initX :: Int
initX = -270

initY :: Int
initY = 220

