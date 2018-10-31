module Main where

data State a = Mine | NotOpen | Cell a deriving(Show)

data Action = Open | Flag deriving(Show)

type Point    = (Int, Int)
type Board    = [[State Int]]
-- type ClueMap  = [[Int]] 
type ExploredBoard = [[State Int]]


size   = 1  -- size of cell
width = 10  -- widht board
height = 10 -- height board
mines = 5 -- amount mines
lastBorderEl = intToDigit (width-1)   -- last number of width
lastHeightEl = intToDigit (height-1)  -- last number of height
borderWidth = ['0'..lastBorderEl]     -- array of indices on width
borderHeight = ['0'..lastHeightEl] 	  -- array of indices on height

main :: IO ()
main = do
  putStrLn "This is saper game!"
