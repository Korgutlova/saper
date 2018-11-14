module Main where

import Data.Char
import System.Random
import Control.Monad
import Types


minesN = 8 -- amount mines

main :: IO ()
main = do
    putStrLn "This is saper game!"
    g <- getStdGen
    let mines = genMinePoints width height minesN g
    putStrLn ("Mines:")
    mapM_ (putStrLn . showTup) mines
    let gameMap = return $ createGameMap width height mines
    mapM_ (putStrLn . unlines) $ map (map show) gameMap


-- random for tuples
instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)

-- generate mines locations
genMinePoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genMinePoints w h n g = take n $ randomRs ((0,0),(w-1,h-1)) $ g :: [(Int,Int)]

-- generate game map
createGameMap :: Int -> Int -> [Point] -> GameMap
createGameMap w h mines = foldr placeMine emptyMap mines
    where
        placeMine point mineMap = сalcCells point (addMine point mineMap)
        addMine point mineMap = changeCell point mineMap (Mine)
        сalcCells point mineMap = foldr сalCell mineMap (surPoints w h point)
        сalCell point@(x, y) mineMap = changeCell point mineMap (incVal (mineMap !! x !! y))
        emptyMap = createEmptyMap w h
        createEmptyMap w h = replicate w $ replicate h $ (Cell 0)



-- incrementing cell value
incVal :: (CellState Int) -> (CellState Int)
incVal (Cell i) = (Cell (i+1))
incVal Mine = Mine


-- return surriunding points
surPoints :: Int -> Int -> Point -> [Point]
surPoints w h (x, y) =
    filter (inBounds w h) [(x-1, y-1), (x, y-1), (x+1, y-1),
                           (x-1, y),             (x+1, y),
                           (x-1, y+1), (x, y+1), (x+1, y+1)]


-- return is point in map bounds
inBounds :: Int -> Int -> Point -> Bool
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True


-- functions for change elem in 2d array
changeCell :: Point -> [[a]] -> a -> [[a]]
changeCell (x, y) map newElem = 
    let modified_row = replace y (map !! x) newElem
    in replace x map modified_row 

replace :: Int -> [a] -> a -> [a]
replace x map newElem = take x map ++ newElem : drop (x+1) map

-- for debug
showTup :: (Show a, Show b) => (a, b) -> String
showTup (a, b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")" 

