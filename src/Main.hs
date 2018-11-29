module Main where

import Types

import Data.Char
import Data.List
import Data.Foldable
import System.Random
import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Juicy

import Data.Time.Clock
import Data.Time.Format


minesN = 15 -- amount mines

main :: IO ()
main = do
        world <- createGame <$> loadImages 
        playIO (InWindow "Sapper game" (800, 700) (300, 100)) black 100 world draw handleEvent handleTime


-- random for tuples
instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)

-- generate mines locations
genMinePoints :: RandomGen gen => Int -> gen -> [Types.Point]
genMinePoints n gen = take n $ nub $ take (n*2) $ randomRs ((0,0),(width-1,height-1)) $ gen :: [(Int,Int)]

-- generate game map
createGameMap :: [Types.Point] -> GameMap
createGameMap mines = foldr placeMine emptyMap mines
    where
        placeMine point mineMap = сalcCells point (addMine point mineMap)
        addMine point mineMap = changeCell point mineMap (Mine)
        сalcCells point mineMap = foldr сalcCell mineMap (surPoints point)
        сalcCell point@(x, y) mineMap = changeCell point mineMap (incVal (mineMap !! x !! y))
        emptyMap = createEmptyMap
        createEmptyMap = replicate width $ replicate height $ (Cell 0)


-- incrementing cell value
incVal :: (CellState Int) -> (CellState Int)
incVal (Cell i) = (Cell (succ i))
incVal Mine = Mine


-- return surriunding points
surPoints :: Types.Point -> [Types.Point]
surPoints (x, y) =
    filter inBounds [(x-1, y-1), (x, y-1), (x+1, y-1),
                     (x-1, y),             (x+1, y),
                     (x-1, y+1), (x, y+1), (x+1, y+1)]


-- return is point in map bounds
inBounds :: Types.Point -> Bool
inBounds (x, y)
    | x < 0         = False
    | x >= width    = False
    | y < 0         = False
    | y >= height   = False
    | otherwise     = True


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
                        (Cell 0) -> foldr (explore gameMap) newExplBoard (surPoints coord) 
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
  <*> fmap fold (loadJuicyPNG "img/minus.png")
  <*> fmap fold (loadJuicyPNG "img/plus.png")
  <*> fmap fold (loadJuicyPNG "img/generate.png")
  <*> fmap fold (loadJuicyPNG "img/restart.png")
  <*> fmap fold (loadJuicyPNG "img/mine.png")


createGame :: Images -> Game
createGame images = Game
    { board = replicate width $ replicate height $ (NotOpen) 
    , closeBoard = replicate width $ replicate height $ (NotOpen)
    , label = "Choose number of mines"  
    , imgs  = images
    , state = Start
    , numMine = 0
    }


draw :: Game -> IO Picture
draw game = do 
                let c   = fromIntegral size
                let s   = 0.096
                let txt = 0.3
                let numb = 0.4
                let x   = fromIntegral initX
                let y   = fromIntegral initY
                let drawLabelStart label = translate (x - 50) (y + 50) 
                                                        (scale numb numb (color white $ text 
                                                            (label)))
                let drawLabelGame label = translate (x - 50) (y + 50) 
                                                        (scale 0.35 0.35 (color white $ text 
                                                            (label)))
                case (state game) of
                    Start -> return (pictures [drawLabelStart (label game)
                                               , translate (x + 110) (y - 130) 
                                                        (scale numb numb (color white $ text 
                                                            (show (numMine game))))
                                               , translate (x + 250) (y - 100) (scale s s (minus (imgs game)))
                                               , translate (x + 250) (y - 160) (scale s s (plus (imgs game)))
                                               , translate (x + 150) (y - 210) (scale 0.2 0.17 (generate (imgs game)))
                                               ])

                    otherwise -> return (pictures 
                                                [ drawEmpty x y s c (open (imgs game))
                                                , drawLabelGame ((label game) ++ " " ++ show((numMine game)))
                                                , drawBoard x y s txt c game
                                                , translate (x + 25) (y -  ((fromIntegral height) + 0.7) * fromIntegral size) 
                                                                            (scale 0.2 0.17 (restart (imgs game)))
                                                , translate (x + 150) (y -  fromIntegral (height + 1) * fromIntegral size) 
                                                            (scale 0.3 0.3 (color white $  text ("Timer: " ++ (format $ timer game))))
                                                ])
                    

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
                            OpenMine   -> translate (x + j * c) (y - i * c) (scale s s (openMine (imgs game)))
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

handleEvent :: Event -> Game -> IO Game
handleEvent (EventKey (Char c) Up _ _) game = castIO (changeMine c game)

handleEvent (EventKey (MouseButton k) Down _ mouse) game = case (state game) of 
                                Start -> case k of 
                                            LeftButton -> checkPushButton mouse game
                                            otherwise -> castIO game
                                Play -> case value of 
                                          (-1, 0) -> castIO game
                                          (0, -1) -> castIO (createGame (imgs game))
                                          otherwise -> case k of 
                                                      LeftButton -> castIO (check (openCellGUI value game))
                                                      RightButton -> castIO (check (setFlag value game))
                                Finish -> case value of 
                                            (0, -1) -> castIO (createGame (imgs game))
                                            otherwise -> castIO game
                                where value = mouseToCell mouse

handleEvent _ w = castIO w

checkPushButton :: G.Point -> Game -> IO Game
checkPushButton (x, y) game     | fromIntegral initX + 225 < x && fromIntegral initX + 275 > x &&
                                  fromIntegral initY - 75 > y && fromIntegral initY - 125 < y = castIO (changeMine '+' game)
                                | fromIntegral initX + 225 < x && fromIntegral initX + 275 > x &&
                                  fromIntegral initY - 135 > y && fromIntegral initY - 185 < y = castIO (changeMine '-' game) 
                                | fromIntegral initX + 115 < x && fromIntegral initX + 215 > x &&
                                  fromIntegral initY - 185 > y && fromIntegral initY - 235 < y = do  getGame game
                                | otherwise = castIO game  

changeMine :: Char -> Game -> Game
changeMine c game = Game 
                     { board = (board game)
                     , closeBoard = (closeBoard game)
                     , label = (label game)
                     , imgs  = (imgs game)
                     , state   = (state game)
                     , numMine = case c of 
                                    '-' -> case n == 0 of 
                                                True -> n
                                                False -> n - 1
                                    '+' -> n + 1
                     , timer = (timer game)
                    }
                  where 
                     n = numMine game 

getGame :: Game -> IO Game
getGame game = do 
            gen <- getStdGen
            time <- getCurrentTime
            let mines = genMinePoints (numMine game) gen
            let gameMap = createGameMap mines
            return Game 
                    { board = (board game)           
                    , closeBoard = gameMap
                    , label = "This is sapper game!"
                    , imgs  = (imgs game)
                    , state   = Play
                    , numMine = (numMine game)
                    , timer = secondsToDiffTime 0
                    }



handleTime :: Float ->  Game -> IO Game
handleTime dsec game = case (state game) of
                                Play -> castIO (incTimer game dsec)
                                otherwise -> castIO game


format :: DiffTime -> String
format dt = show min ++ ":" ++ sec_show sec 
              where
                sec_show sec  | sec < 10  = "0" ++ show sec
                              | otherwise = show sec
                min = floor dt `div` 60
                sec = floor dt `mod` 60


seconds2DiffTime :: Float -> DiffTime
seconds2DiffTime = realToFrac 


incTimer :: Game -> Float -> Game
incTimer  game dsec = Game
                    {  board = (board game)          
                     , closeBoard = (closeBoard game)
                     , label = (label game)
                     , imgs  = (imgs game)
                     , state   = (state game)
                     , numMine = (numMine game)
                     , timer = (timer game) + (seconds2DiffTime dsec)
                    }



mouseToCell :: G.Point  -> (Int, Int)
mouseToCell (x, y) = case (i > -1 && i < height && j > -1 && j < height) of 
                          True -> (j, i)
                          False -> case (custX - 25 < x && custX + 120 > x && custY + 25 > y && custY - 5 < y) of  
                                    True -> (0, -1)
                                    False -> (-1, 0)
        where   
            i = floor (x - custX + delta) `div` size          
            j = floor (fromIntegral height - y + fromIntegral initY + delta) `div` size
            custX = fromIntegral initX
            custY = fromIntegral initY - ((fromIntegral height) + 0.7) * fromIntegral size 

openCellGUI :: (Int, Int) -> Game -> Game
openCellGUI (x, y) game = case elem of  
                    NotOpen -> case checkMine (x, y) (closeBoard game) of
                                    True -> Game
                                        {  board = changeCell (x, y) (changeMineToFlag b (closeBoard game)) OpenMine
                                         , closeBoard = (closeBoard game)
                                         , label = "  GAME OVER!!!  "
                                         , imgs  = (imgs game)
                                         , state   = Finish
                                         , numMine = 0
                                         , timer = (timer game)
                                        }
                                    False -> Game 
                                        { board = openCell (closeBoard game) (x, y) b           
                                         , closeBoard = (closeBoard game)
                                         , label = (label game)
                                         , imgs  = (imgs game)
                                         , state   = (state game)
                                         , numMine = (numMine game)
                                         , timer = (timer game)
                                        }
                    otherwise -> game
                where 
                    elem = b !! x !! y
                    b = (board game)

--implement

changeMineToFlag :: ExploredBoard -> GameMap -> GameMap
changeMineToFlag explBoard gameMap = gameMap

changeMineToFlagCoord :: ExploredBoard -> GameMap -> Types.Point -> GameMap
changeMineToFlagCoord explBoard gameMap coord = case getCell coord explBoard of 
                                                    MineFlag -> case (getCell coord gameMap == Mine) of 
                                                                True -> changeCell coord gameMap MineFlag
                                                                False -> gameMap 
                                                    otherwise -> gameMap

setFlag :: (Int, Int) -> Game -> Game
setFlag (x, y) game = Game 
                    { board = case elem of 
                              NotOpen ->  case n == 0 of 
                                        False -> changeCell (x, y)  b (MineFlag)
                                        True -> b
                              MineFlag -> changeCell (x, y)  b (NotOpen)
                              otherwise -> b
                    , closeBoard = (closeBoard game)
                    , label = (label game)
                    , imgs  = (imgs game)
                    , state   = (state game)
                    , numMine = case elem of 
                              NotOpen -> case n == 0 of
                                        True -> n  
                                        False -> n - 1
                              MineFlag -> n + 1
                              otherwise -> n
                    , timer = (timer game)
                    }
                    where 
                        b = (board game)
                        elem = b !! x !! y
                        n = (numMine game)

check :: Game -> Game
check game = case (state game) of 
            Play -> case checkBoard (board game) (closeBoard game) of 
                    True -> Game 
                          { closeBoard = (closeBoard game)
                          , board = (board game)
                          , label = "  YOU WIN!!!  "
                          , imgs  = (imgs game)
                          , state   = Finish
                          , numMine = 0
                          , timer = (timer game)
                          }
                    False -> game
            otherwise -> game
        
castIO :: Game -> IO Game
castIO game = do 
                return game
