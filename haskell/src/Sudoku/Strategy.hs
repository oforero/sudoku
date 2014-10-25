module Sudoku.Strategy where

import           Sudoku.Types (Board, Cell, Options (..), cells, isUnsolvable,
                               possible, simplifyBoard)

type Strategy = (Board -> Board)

getCells :: (Options -> Bool) -> Board -> [(Cell, Options)]
getCells p b = filter p' $ map zipper cells
    where
        zipper c = (c, possible c b)
        p' (_, o) = p o

-- Strategies can be applied recursively
-- This function applies a strategy until it has no effect in the board
-- or finds it results in an unsolvable board,
-- then return the resulting board
applyStrategy :: Strategy -> Board -> Board
applyStrategy = applyStrategy' False
    where
        applyStrategy' True _ b = b
        applyStrategy' False s b = applyStrategy' (isUnsolvable b' || b == b') s b'
            where
                b' = s b

-- Strategy for singles: http://hodoku.sourceforge.net/en/tech_singles.php --
-- There are 3 known strategies to deal with singles in Sudoku: Full House, Hidden Single, Naked Single
-- For the representation I chose they are basically the same strategy (repeated application of Naked Single)
-- Given a cell with a single option, eliminate that option from the row, column and box that cell belongs to
-- To implement that Strategy in a given board first I identify a cell having a single option
-- Then call simplify board with all the singles

isSingle :: Options -> Bool
isSingle (Options [_]) = True
isSingle  _  = False

nakedSingle :: Strategy
nakedSingle b = simplifyBoard b $ getCells isSingle b


-- Other Strategies


-- Search
