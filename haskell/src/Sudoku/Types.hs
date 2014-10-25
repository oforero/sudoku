module Sudoku.Types (Value, Options(..), Board, Cell, Unit,
                     readBoard, showBoard, showBoard', cells, allUnits, possible, simplifyBoard) where

import           Data.Char (digitToInt)
import           Data.List (union, (\\))


-- Use a data type instead of integers, to avoid ending up with wrong values in a cell
data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord)

-- All possible values
values :: [Value]
values = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

instance Enum Value where
    toEnum n  = values !! (n-1)
    fromEnum e  = el values e 1
        where
            el (v:vs) w n
               | v == w = n
               | otherwise = el vs w (n+1)

instance Show Value where
    show e = show $ fromEnum e

-- Use a datatype to represent the possible values a cell can take
-- A sudoku board will be solve if all cells have only one possible value (that follows the rules)
-- and will be impossible to solve if one of the cells have empty options
data Options = Options [Value] deriving Eq
instance Show Options where
    show (Options [])  = "⊥"
    show (Options [n]) = show n
    show (Options xs)  = nums !! ((length xs) - 1)
        where
            nums = ["", "B", "C", "D", "E", "F", "G", "H", "I"]


diff :: Options -> Options -> Options
(Options vs) `diff` (Options ws) = Options $ vs \\ ws

{-
     A  B  C   D  E  F   G  H  I
     0  1  2 | 3  4  5 | 6  7  8    1
     9 10 11 |12 13 14 |15 16 17    2
    18 19 20 |21 22 23 |24 25 26    3
    ---------|---------|--------
    27 28 29 |30 31 32 |33 34 35    4
    36 37 38 |39 40 41 |42 43 44    5
    45 46 47 |48 49 50 |51 52 53    6
    ---------|---------|--------
    54 55 56 |57 58 59 |60 61 62    7
    63 64 65 |66 67 68 |69 70 71    8
    72 73 74 |75 76 77 |78 79 80    9
-}

-- I'll keep the board as a single row of 80 possible values,
-- The possible values are maintained as a list of elements
-- An empty list means that no assignment is possible
type Cell = Int
type Unit = [Cell]

cells :: [Cell]
cells = [0..80]

-------

data Board = BoardC [Options] deriving Eq

fromInt :: Int -> Options
fromInt 0 = Options values
fromInt n = Options [toEnum n]

fromList :: [Int] -> Board
fromList xs = BoardC $ map fromInt xs

readInts :: String -> [Int]
readInts = map digitToInt

readBoard :: String -> Board
readBoard = fromList . readInts

possible :: Cell -> Board -> Options
possible c (BoardC xs) = xs !! c

-- Sudoku players call the rows, columns and boxes units
-- Write some utility functions to map a cell from the board to a unit
rowS :: [Cell]
rowS = [0,9..72]
allRows :: [Unit]
allRows = map row rowS
    where
        row c = [c..c+8]

colS :: [Cell]
colS = [0..8]
allCols :: [Unit]
allCols = map col colS
    where
        col c = [c, c+9 .. c+72]

box1 :: Unit
box1 =  [0,  1,  2,
         9, 10, 11,
        18, 19, 20]

allBoxes :: [Unit]
allBoxes = map (buildBox box1) [0, 3, 6, 27, 30, 33, 54, 57, 60]
     where
         buildBox b i = map (i+) b

allUnits :: [Unit]
allUnits = allRows ++ allCols ++ allBoxes

isNeighborOf :: Cell -> Cell -> Bool
isNeighborOf c c' = any (bothIn c c') allUnits
    where
        bothIn c c' u = c `elem` u && c' `elem` u

simplify :: (Cell, Options) -> [Options] -> [Options]
simplify (ex, rs) opss = map (removeIfNeighbor ex rs opss) cells
    where
        removeIfNeighbor :: Cell -> Options -> [Options] -> Cell -> Options
        removeIfNeighbor c rs opss c'
            | c == c' = opss !! c'
            | c `isNeighborOf` c' = (opss !! c') `diff` rs
            | otherwise = opss !! c'

simplifyBoard :: Board ->  [(Cell, Options)] -> Board
simplifyBoard (BoardC opss) rs = BoardC $ foldr simplify opss rs

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
showValue (BoardC sb) c = " " ++ show (sb !! c) ++ " "

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
