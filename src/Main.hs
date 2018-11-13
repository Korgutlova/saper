module Main where

import Data.Char
import System.Random
import Control.Monad
import Types
import Pics

minesN = 5 -- amount mines

main :: IO ()
main = startGame
-- main = do
--     putStrLn "This is saper game!"
--     g <- getStdGen
--     let mines = genMinePoints width height minesN g
--     putStrLn ("Mines:")
--     mapM_ (putStrLn . showTup) mines
--  gameBoard <- return $ genGameBoard width height mines

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)

showTup :: (Show a, Show b) => (a,b) -> String
showTup (a,b) = "(" ++ (show a) ++ "," ++ (show b) ++ ")" 

genMinePoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genMinePoints w h n g = take n $ randomRs ((0,0),(w,h)) $ g :: [(Int,Int)]
