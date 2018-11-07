module Types where

data CellState a = Mine | NotOpen | Flag | Open a deriving(Show)

-- data Action = Open | Flag deriving(Show)

type Point    = (Int, Int)
type Board    = [[CellState Int]]
-- type ClueMap  = [[Int]]