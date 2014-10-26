module Main where

import           Data.Char          (isSpace)
import           Data.List.Split    (chunksOf)
import           System.Environment (getArgs)

import           Sudoku.Board       (Board, readBoard, showBoard, showBoard',
                                     showBoardCsv)
import           Sudoku.Strategy

sudoku :: [String] -> IO ()
sudoku ["-h"] = sudoku []
sudoku ["-s", "-b", s] = sudoku' False $ readBoard s
sudoku ["-b", s] = sudoku' True $ readBoard s
sudoku ["-s", "-f", file] = do
     b <- fromFile [] file
     sudoku' False b
sudoku ["-f", file] = do
     b <- fromFile [] file
     sudoku' True b
sudoku ["-s", "-csv", file] = do
     b <- fromCsvFile file
     sudoku' False b
sudoku ["-p", "-csv", file] = do
     b <- fromCsvFile file
     sudoku' True b
sudoku ["-csv", file] = do
     b <- fromCsvFile file
     sudoku'' b

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

sudoku'' b = do
     let s = solve b
     printB s
     where
         printB Unsolvable   = putStr "Unsolvable"
         printB (Solution s) = printBoardCsv s


printBoard :: Board -> IO ()
printBoard mb = mapM_ putStrLn $ showBoard mb

printBoard' :: Board -> IO ()
printBoard' mb = mapM_ putStrLn $ showBoard' mb

printBoardCsv :: Board -> IO ()
printBoardCsv mb = mapM_ putStrLn $ showBoardCsv mb

fromFile :: [Char -> Bool] -> String -> IO Board
fromFile ignore f = do
     contents <- readFile f
     return $ readBoard $ filter rules contents
         where
             rules' = (not.isSpace):ignore
             rules c = all (True ==) $ map (\p -> p c) rules'

fromCsvFile :: String -> IO Board
fromCsvFile = fromFile [(\c -> c /= ',')]

main :: IO ()
main = do
     args <- getArgs
     sudoku args
