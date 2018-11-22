module Types where

import Data.Char
import Graphics.Gloss

data CellState a = Mine | NotOpen | MineFlag | Cell a deriving(Show, Eq)

type Point          = (Int, Int)
type GameMap        = [[CellState Int]]
type ExploredBoard  = [[CellState Int]]

data Game = Game 
            { closeBoard :: GameMap
            , board :: ExploredBoard
            , label :: String
            , imgs  :: Images
            , win   :: Bool
            , numMine :: Int
            }

data Images = Images 
            { mine  :: Picture
            , flag  :: Picture 
            , block :: Picture
            , open  :: Picture
            }

size :: Int
size = 50 -- size cell

width :: Int
width = 10  -- widht board

height :: Int
height = 10 -- height board


initX :: Int
initX = -225

initY :: Int
initY = 220

delta :: Float
delta = 25

