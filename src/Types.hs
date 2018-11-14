module Types where

import Data.Char

data CellState a = Mine | NotOpen | MineFlag | Cell a deriving(Show)

-- data Action = Open | Flag deriving(Show)

type Point          = (Int, Int)
type GameMap        = [[CellState Int]]
type ExploredBoard  = [[CellState Int]]
-- type ClueMap     = [[Int]] 

size :: Int
size = 50 -- size cell

width :: Int
width = 10  -- widht board

height :: Int
height = 10 -- height board

lastBorderEl    = intToDigit (width-1)   -- last number of width
lastHeightEl    = intToDigit (height-1)  -- last number of height
borderWidth     = ['0'..lastBorderEl]     -- array of indices on width
borderHeight    = ['0'..lastHeightEl]    -- array of indices on height


