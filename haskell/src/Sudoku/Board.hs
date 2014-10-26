module Sudoku.Board (Board, Cell, Options, cells, readBoard, showBoard,
                     showBoard', isUnsolvable, isSolvable,
                     isSolution, isAssigned, expandBoard,
                     possible, simplifyBoard) where

import           Data.Char    (digitToInt)
import           Data.List    (findIndex)
import           Sudoku.Types (Cell, House, Options, cells, diff, expand,
                               fromInt, isAssigned, isEmpty, isExpandable)

-- A Board will look like this, with a cell beloging either 3 houses
-- One coulumn, One row and One Box
-- Instead of maintaining a very complex data structure, I decided to keep
-- the board in a single list of 81 elements and determine the neighboors
-- using utility functions.
{-
     0  1  2   3  4  5   6  7  8

     0  1  2 | 3  4  5 | 6  7  8    0
     9 10 11 |12 13 14 |15 16 17    1
    18 19 20 |21 22 23 |24 25 26    2
    ---------|---------|--------
    27 28 29 |30 31 32 |33 34 35    3
    36 37 38 |39 40 41 |42 43 44    4
    45 46 47 |48 49 50 |51 52 53    5
    ---------|---------|--------
    54 55 56 |57 58 59 |60 61 62    6
    63 64 65 |66 67 68 |69 70 71    7
    72 73 74 |75 76 77 |78 79 80    8
-}

-- I'll keep the board as a single row of 80 possible values,
-- The possible values are maintained as a list of elements
-- An empty list means that no assignment is possible

data Board = Board [Options] deriving Eq

fromList :: [Int] -> Board
fromList xs = Board $ map fromInt xs

readInts :: String -> [Int]
readInts = map digitToInt

readBoard :: String -> Board
readBoard = fromList . readInts

possible :: Cell -> Board -> Options
possible c (Board xs) = xs !! c

-- Sudoku players call the rows, columns and boxes units
-- The following are utility functions that map a cell in a flat board
-- to a House, in this way the Board itself does not have to duplicate that
-- information in each instance
rowS :: [Cell]
rowS = [0,9..72]
allRows :: [House]
allRows = map row rowS
    where
        row c = [c..c+8]

colS :: [Cell]
colS = [0..8]
allCols :: [House]
allCols = map col colS
    where
        col c = [c, c+9 .. c+72]

box1 :: House
box1 =  [0,  1,  2,
         9, 10, 11,
        18, 19, 20]

allBoxes :: [House]
allBoxes = map (buildBox box1) [0, 3, 6, 27, 30, 33, 54, 57, 60]
     where
         buildBox b i = map (i+) b

allHouses :: [House]
allHouses = allRows ++ allCols ++ allBoxes

-- Using the above mappings, the following function will, given a cell,
-- return a list of cells that share a house with it
isNeighborOf :: Cell -> Cell -> Bool
isNeighborOf c c' = any (bothIn c c') allHouses
    where
        bothIn c c' u = c `elem` u && c' `elem` u

-- Given a pair of cell and a list of values to remove, and a list of Options
-- return a new list of possible values (Options) with the values removed
-- from the position corresponding to neighbouring cells
simplify :: (Cell, Options) -> [Options] -> [Options]
simplify (ex, rs) opss = map (removeIfNeighbor ex rs opss) cells
    where
        removeIfNeighbor :: Cell -> Options -> [Options] -> Cell -> Options
        removeIfNeighbor c rs opss c'
            | c == c' = opss !! c'
            | c `isNeighborOf` c' = (opss !! c') `diff` rs
            | otherwise = opss !! c'

simplifyBoard :: Board ->  [(Cell, Options)] -> Board
simplifyBoard (Board opss) rs = Board $ foldr simplify opss rs

-- Some Predicates over boards useful for writing the solver
isUnsolvable :: Board -> Bool
isUnsolvable (Board opss) = any isEmpty opss

isSolvable :: Board -> Bool
isSolvable = not . isUnsolvable

isSolution :: Board -> Bool
isSolution (Board opss) = all isAssigned opss

-- Given a board, find the first cell with that can take more than one possible value
-- and return a list of Boards, each element of the list will be a board with the cell
-- set to a single one of the possible values it can take
expandBoard :: Board -> [Board]
expandBoard (Board opss) = newBoard $ findIndex isExpandable opss
    where
        newBoard :: Maybe Int -> [Board]
        newBoard Nothing  = [Board opss]
        newBoard (Just n) = map (builder n) $ expand (opss !! n)
        builder n v = Board $ take n opss ++ v:drop (n+1) opss

-- Haskell does not support String interpolation out of the box,
-- so I faked it by mapping over the following string.
board :: [String]
board = [
        "‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒",
         "0  1  2 |  3  4  5 |  6  7  8",
         "9 10 11 | 12 13 14 | 15 16 17",
        "18 19 20 | 21 22 23 | 24 25 26",
        "‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒",
        "27 28 29 | 30 31 32 | 33 34 35",
        "36 37 38 | 39 40 41 | 42 43 44",
        "45 46 47 | 48 49 50 | 51 52 53",
        "‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒",
        "54 55 56 | 57 58 59 | 60 61 62",
        "63 64 65 | 66 67 68 | 69 70 71",
        "72 73 74 | 75 76 77 | 78 79 80",
        "‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒"
        ]

showValue :: Board -> Int -> String
showValue (Board sb) c = " " ++ show (sb !! c) ++ " "

showBoard :: Board -> [String]
showBoard b = map showRow board
    where
        showCell :: String -> String
        showCell "|" = "|"
        showCell ws@('‒':_) = ws
        showCell s =  showValue b i
           where
               i = read s :: Int
        showRow r = unwords $ map showCell $ words r

showBoard' :: Board -> [String]
showBoard' b = map (map (replace ['B'..'I'] '•')) $ showBoard b
    where
        replace rs v c
            | c `elem` rs = v
            | otherwise = c
