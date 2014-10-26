module Main where

import           Data.Char          (isSpace)
import           Data.List.Split    (chunksOf)
import           System.Environment (getArgs)

import           Sudoku.Board       (Board, readBoard, showBoard, showBoard')
import           Sudoku.Strategy

sudoku :: [String] -> IO ()
sudoku ["-h"] = sudoku []
sudoku ["-s", "-b", s] = sudoku' False $ readBoard s
sudoku ["-b", s] = sudoku' True $ readBoard s
sudoku ["-s", "-f", file] = do
     b <- fromFile file
     sudoku' False b
sudoku ["-f", file] = do
     b <- fromFile file
     sudoku' True b
sudoku _ = putStrLn "Call the program with [-b|-s] <String|File>"

sudoku' :: Bool -> Board -> IO ()
sudoku' True b = do
     let s = solve b
     printBoard' b
     putStrLn ""
     -- printBoard $ applyStrategy nakedSingle $ applyStrategy nakedSingle g
     printB s
     where
         printB Unsolvable   = putStrLn "Unsolvable"
         printB (Solution s) = printBoard' s

sudoku' False b = do
     let s = solve b
     printB s
     where
         printB Unsolvable   = putStr "Unsolvable"
         printB (Solution s) = putStr "Solved"

printBoard :: Board -> IO ()
printBoard mb = mapM_ putStrLn $ showBoard mb

printBoard' :: Board -> IO ()
printBoard' mb = mapM_ putStrLn $ showBoard' mb

fromFile :: String -> IO Board
fromFile f = do
     contents <- readFile f
     return $ readBoard $ filter (not . isSpace) contents

main :: IO ()
main = do
     args <- getArgs
     sudoku args
