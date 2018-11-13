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
            , win   :: Bool
            , imgs  :: Images
            }

data Images = Images 
            { mina :: Picture
            , flag :: Picture 
            }

loadImages :: IO Images
loadImages = Images
  <$> fmap fold (loadJuicyPNG "img/bomb.png")
  <*> fmap fold (loadJuicyPNG "img/flag.png")

example = [ [Mine, Cell 1, Cell 2, Cell 3], 
          [NotOpen, Cell 3, Cell 4, Mine],
          [Cell 1, Cell 2, Cell 3, Mine]] 

initialWorld :: IO Game
initialWorld = createGame <$> loadImages

createGame :: Images -> Game
createGame images = Game
    { board = example 
    , win   = False
    , imgs  = images
    }

draw :: Game -> Picture
draw game = pictures 
    [ translate (-250) (-200) (scale c c (drawGrid))
    , translate (-230) (-180) (scale 0.1 0.1 (mina (imgs game)))
    , translate (-230) (-80) (scale 0.1 0.1 (flag (imgs game)))]
    where
    c = fromIntegral size

changeColor :: Picture
changeColor = translate (-100) (-100) $ color green $ text ("Hi")

drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [0..m]
    vs = map (\i -> line [(i, 0), (i, m)]) [0..n]

    n = fromIntegral width
    m = fromIntegral height

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) game = openCell (mouseToCell mouse) game
handleEvent _ w = w

handleTime :: Float -> Game -> Game
handleTime _ w = w

logInfo :: Types.Point -> IO ()
logInfo (i, j) = do 
                    putStrLn("Point " ++ show (i,j))  
                    -- putStrLn(show j)

mouseToCell :: G.Point  -> IO (Int, Int)
mouseToCell (x, y) = do    
                        let i = floor (x + fromIntegral screenWidth) `div` size
                        let j = floor (y + fromIntegral screenHeight) `div` size
                        putStrLn ("Point "  ++ show (i, j)) 
                        return (i, j) 


openCell :: IO (Int, Int) -> Game -> Game
openCell _ game = game
                    -- translate (-10) 50 $ color green $ text (i, j)
                    -- return game
-- openCell _ game = game

showTup2 :: (Show a, Show b) => (a,b) -> String
showTup2 (a,b) = "Point (" ++ (show a) ++ "," ++ (show b) ++ ")" 
-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = size * width

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = size * height 


