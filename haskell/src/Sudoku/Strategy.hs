module Sudoku.Strategy where

import           Sudoku.Types (Board, Cell, Options (..), cells, possible,
                               simplifyBoard)

type Strategy = (Board -> Board)


isSingle :: Options -> Bool
isSingle (Options [_]) = True
isSingle  _  = False


getCells :: (Options -> Bool) -> Board -> [(Cell, Options)]
getCells p b = filter p' $ map zipper cells
    where
        zipper c = (c, possible c b)
        p' (_, o) = p o

applyStrategy :: Strategy -> Board -> Board
applyStrategy = applyStrategy' False
    where
        applyStrategy' True _ b = b
        applyStrategy' False s b = applyStrategy' (b == b') s b'
            where
                b' = s b

-- Hidden Single --
-- When a number is the only option in a cell it can't be assigned to any other cell in the same unit
-- Create a new board by eliminating the number from the possible values each cell can take and generate a new board
hiddenSingle :: Strategy
hiddenSingle b = simplifyBoard b $ getCells isSingle b
