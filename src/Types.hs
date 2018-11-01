module Types where

data CellState a = Mine | NotOpen | Flag | Open a deriving(Show)

-- data Action = Open | Flag deriving(Show)

type Point    = (Int, Int)
type BoardMap    = [[CellState Int]]
type ExploredBoard = [[CellState Int]]
-- type ClueMap  = [[Int]] 