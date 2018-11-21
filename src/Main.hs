module Main where

import Types

import Data.Char
import Data.List
import Data.Foldable
import System.Random
import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as G
import Graphics.Gloss.Juicy


minesN = 15 -- amount mines

main :: IO ()
main = do
        g <- getStdGen
        let mines = genMinePoints width height minesN g
        -- mapM_ (putStrLn . showTup) mines
        let gameMap = createGameMap width height mines
        -- mapM_ (putStrLn . unlines) $ map (map show) gameMap
        world <- createGame gameMap <$> loadImages
        play FullScreen blue 100 world draw handleEvent handleTime


-- random for tuples
instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)

-- generate mines locations
genMinePoints :: RandomGen g => Int -> Int -> Int -> g -> [Types.Point]
genMinePoints w h n g = nub $ take n $ randomRs ((0,0),(w-1,h-1)) $ g :: [(Int,Int)]

-- generate game map
createGameMap :: Int -> Int -> [Types.Point] -> GameMap
createGameMap w h mines = foldr placeMine emptyMap mines
    where
        placeMine point mineMap = сalcCells point (addMine point mineMap)
        addMine point mineMap = changeCell point mineMap (Mine)
        сalcCells point mineMap = foldr сalcCell mineMap (surPoints w h point)
        сalcCell point@(x, y) mineMap = changeCell point mineMap (incVal (mineMap !! x !! y))
        emptyMap = createEmptyMap w h
        createEmptyMap w h = replicate w $ replicate h $ (Cell 0)


-- incrementing cell value
incVal :: (CellState Int) -> (CellState Int)
incVal (Cell i) = (Cell (succ i))
incVal Mine = Mine


-- return surriunding points
surPoints :: Int -> Int -> Types.Point -> [Types.Point]
surPoints w h (x, y) =
    filter (inBounds w h) [(x-1, y-1), (x, y-1), (x+1, y-1),
                           (x-1, y),             (x+1, y),
                           (x-1, y+1), (x, y+1), (x+1, y+1)]


-- return is point in map bounds
inBounds :: Int -> Int -> Types.Point -> Bool
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True


-- functions for change elem in 2d array
changeCell :: Types.Point -> [[a]] -> a -> [[a]]
changeCell (x, y) map newElem = 
    let modified_row = replace y (map !! x) newElem
    in replace x map modified_row 

replace :: Int -> [a] -> a -> [a]
replace x map newElem = take x map ++ newElem : drop (x+1) map

-- print tuples
showTup :: (Show a, Show b) => (a, b) -> String
showTup (a, b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")"


-- check the board on the opening of all cells 
checkBoard :: ExploredBoard -> GameMap -> Bool
checkBoard explBoard gameMap = all isOpen coords
                        where
                          coords = [(i,j) | i <- [0..(width-1)], j <-  [0..(height-1)]]
                          isOpen coord = 
                            (isCell (getCell coord explBoard)) || (getCell coord gameMap == Mine)
                          isCell (Cell _) = True
                          isCell _ = False

-- open closed cell in explored board
openCell :: GameMap -> Types.Point -> ExploredBoard -> ExploredBoard
openCell gameMap coord explBoard = case cell of 
                        (Cell 0) -> foldr (explore gameMap) newExplBoard (surPoints width height coord) 
                        otherwise -> newExplBoard
                        where
                          cell = getCell coord gameMap
                          newExplBoard = changeCell coord explBoard cell


-- exploring is cell open or not
explore :: GameMap -> Types.Point -> ExploredBoard -> ExploredBoard
explore gameMap coord explBoard = case cell of
                        NotOpen -> openCell gameMap coord explBoard
                        otherwise -> explBoard
                        where
                          cell = getCell coord explBoard


checkMine :: Types.Point -> GameMap -> Bool
checkMine coord gameMap = case (getCell coord gameMap) of 
                        Mine -> True
                        otherwise -> False


getCell :: Types.Point -> GameMap -> (CellState Int)
getCell coord gameMap = gameMap !! (fst coord) !! (snd coord)


loadImages :: IO Images
loadImages = Images
  <$> fmap fold (loadJuicyPNG "img/bomb.png")
  <*> fmap fold (loadJuicyPNG "img/flag.png")
  <*> fmap fold (loadJuicyPNG "img/block.png")
  <*> fmap fold (loadJuicyPNG "img/open.png")

createGame :: GameMap -> Images -> Game
createGame gamemap images = Game
    { board = replicate width $ replicate height $ (NotOpen) 
    , closeBoard =gamemap
    , label = "This is saper game!" 
    , imgs  = images
    , win = False
    }

draw :: Game -> Picture
draw game = pictures 
    [ drawEmpty x y s c (open (imgs game))
    , drawLabel (label game)  
    , drawBoard x y s txt c game]
    where
        drawLabel label = translate (x) (y + 50) 
                                        (scale txt txt (color black $ text label))
        c   = fromIntegral size
        s   = 0.096
        txt = 0.3
        x   = fromIntegral initX
        y   = fromIntegral initY

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

    n  = fromIntegral width
    m  = fromIntegral height

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
                            NotOpen    -> translate (x + j * c) (y - i * c) (scale s s (block (imgs game)))
                            Mine       -> translate (x + j * c) (y - i * c) (scale s s (mine (imgs game)))
                            MineFlag   -> translate (x + j * c) (y - i * c) (scale s s (flag (imgs game)))
                            Cell 0     -> translate (x + j * c) (y - i * c) (scale s s (open (imgs game)))
                            Cell z     -> translate (x + j * c - deltax) (y - i * c - deltay) 
                                        (scale txt txt (color black $ text (show z))))
            (generateArray (n, m) m n)
        cust_map = (board game)
        n        = fromIntegral width - 1
        m        = fromIntegral height - 1
        deltax   = fromIntegral 10
        deltay   = fromIntegral 15

getElem :: ExploredBoard -> Float -> Float -> CellState Int
getElem cust_map i j = cust_map !! (round i) !! (round j)

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton k) Down _ mouse) game = case (win game) of 
                                  False -> case value of 
                                          (-1, -1) -> game
                                          otherwise -> case k of 
                                                      LeftButton -> check (openCellGUI value game)
                                                      RightButton -> check (setFlag value game)
                                          where value = mouseToCell mouse
                                  True -> game
handleEvent _ w = w

handleTime :: Float -> Game -> Game
handleTime _ w = w

mouseToCell :: G.Point  -> (Int, Int)
mouseToCell (x, y) = case (i > -1 && i < height && j > -1 && j < height) of 
                          True -> (j, i)
                          False -> (-1, -1)
        where   
            i = floor (x - fromIntegral initX + delta) `div` size          
            j = floor (fromIntegral height - y + fromIntegral initY + delta) `div` size 

openCellGUI :: (Int, Int) -> Game -> Game
openCellGUI (x, y) game = case elem of  
                    NotOpen -> case checkMine (x, y) (closeBoard game) of
                                    True -> Game
                                        {  board = (closeBoard game)
                                         , closeBoard = (closeBoard game)
                                         , label = "GAME OVER!!"
                                         , imgs  = (imgs game)
                                         , win   = True
                                        }
                                    False -> Game 
                                        { board = openCell (closeBoard game) (x, y) b           
                                         , closeBoard = (closeBoard game)
                                         , label = (label game)
                                         , imgs  = (imgs game)
                                         , win   = (win game)
                                        }
                    otherwise -> game
                where 
                    elem = b !! x !! y
                    b = (board game)
setFlag :: (Int, Int) -> Game -> Game
setFlag (x, y) game = Game 
                    { board = case b !! x !! y of 
                              NotOpen -> changeCell (x, y)  b (MineFlag)
                              MineFlag -> changeCell (x, y)  b (NotOpen)
                              otherwise -> b
                    , closeBoard = (closeBoard game)
                    , label = (label game)
                    , imgs  = (imgs game)
                    , win   = (win game)
                    }
                    where b = (board game)

check :: Game -> Game
check game = case (win game) of 
            False -> case checkBoard (board game) (closeBoard game) of 
                    True -> Game 
                          { closeBoard = (closeBoard game)
                          , board = (board game)
                          , label = "YOU WIN!!!"
                          , imgs  = (imgs game)
                          , win   = True
                          }
                    False -> game
            True -> game
              