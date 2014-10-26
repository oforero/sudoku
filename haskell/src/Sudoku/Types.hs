module Sudoku.Types (Cell, House, Value, Options,
                     cells, fromInt, diff, isExpandable,
                     isAssigned, isEmpty, expand) where

import           Data.List ((\\))

type Cell = Int
type House = [Cell]

-- I use this to map/iterate over a Board without the need to use a for/while loop
cells :: [Cell]
cells = [0..80]
-------


-- Use a data type instead of integers, to avoid ending up with wrong values in a cell
data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Eq, Ord)

-- All possible possibleCellValues
possibleCellValues :: [Value]
possibleCellValues = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

instance Enum Value where
    toEnum n  = possibleCellValues !! (n-1)
    fromEnum e  = el possibleCellValues e 1
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

fromInt :: Int -> Options
fromInt 0 = Options possibleCellValues
fromInt n = Options [toEnum n]

diff :: Options -> Options -> Options
(Options vs) `diff` (Options ws) = Options $ vs \\ ws

isEmpty :: Options -> Bool
isEmpty (Options []) = True
isEmpty _ = False

isAssigned :: Options -> Bool
isAssigned (Options [_]) = True
isAssigned _ = False

isExpandable :: Options -> Bool
isExpandable (Options []) = False
isExpandable (Options [_]) = False
isExpandable _ = True

expand :: Options -> [Options]
expand (Options vs) = map builder vs
    where
        builder v = Options [v]
