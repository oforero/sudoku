module Sudoku.Strategy where

import           Sudoku.Types (Board, Cell, Options (..), cells, expandBoard,
                               isAssigned, isSolution, isSolvable, isUnsolvable,
                               possible, simplifyBoard)

import           Data.List    (findIndex)
import           Data.Maybe   (fromJust)

type Strategy = (Board -> Board)

data Solution = Unsolvable | Solution Board

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
        applyStrategy' True  _ b = b
        applyStrategy' False s b
            | isSolution b = b
            | otherwise = applyStrategy' (isUnsolvable b' || b == b') s b'
                where
                    b' = s b

-- Strategy for singles: http://hodoku.sourceforge.net/en/tech_singles.php --
-- There are 3 known strategies to deal with singles in Sudoku: Full House, Hidden Single, Naked Single
-- For the representation I chose they are basically the same strategy (repeated application of Naked Single)
-- Given a cell with a single option, eliminate that option from the row, column and box that cell belongs to
-- To implement that Strategy in a given board first I identify a cell having a single option
-- Then call simplify board with all the singles
isSingle :: Options -> Bool
isSingle = isAssigned

nakedSingle :: Strategy
nakedSingle b = simplifyBoard b $ getCells isSingle b


-- Other Strategies


-- Search
search :: Board -> [Board]
search b = search' [b]
    where
        search'' b = filter isSolvable $ map (applyStrategy nakedSingle) $ expandBoard $ applyStrategy nakedSingle b

        search' [] = []
        search' (b:bs)
            | isSolution b = [b]
            | otherwise    = search' $ search'' b ++ bs

solve :: Board -> Solution
solve = solution . search
    where
        solution [] = Unsolvable
        solution [b] = Solution b
