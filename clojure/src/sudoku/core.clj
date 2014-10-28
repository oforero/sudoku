(ns sudoku.core
  (:gen-class)
  (:use [sudoku.board :only [read-board, show-board-pretty]]
        [sudoku.types :only [show-value]]
        [sudoku.strategy :only [solve]]))

(defn -main
  "I don't do a whole lot ... yet."
  [b & args]
  (println (show-board-pretty (read-board b)))
  (println (show-board-pretty (solve (read-board b))))
)

(def test-board-1 (read-board "003020600900305001001806400008102900700000008006708200002609500800203009005010300"))
(def test-board-2 (read-board "200080300060070084030500209000105408000000000402706000301007040720040060004010003"))
