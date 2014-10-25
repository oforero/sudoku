module Main where

import           Data.Char          (isSpace)
import           Data.List.Split    (chunksOf)
import           System.Environment (getArgs)

import           Sudoku.Strategy
import           Sudoku.Types       (Board, readBoard, showBoard, showBoard')

sudoku :: [String] -> IO ()
sudoku ["-h"] = putStrLn "I will give you some help"
-- sudoku ["-g", s] = print $ show $ readBoard s
sudoku [file] = do
     g <- fromFile file
     -- let g'  = nakedSingle g
     -- let g'' = applyStrategy nakedSingle g
     -- let g''' = search g''
     let s = solve g
     printBoard' g
     putStrLn ""
     -- printBoard $ applyStrategy nakedSingle $ applyStrategy nakedSingle g
     printB s
     where
         printB Unsolvable   = putStrLn "Unsolvable"
         printB (Solution s) = printBoard' s


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
