(ns sudoku.core
  (:gen-class)
  (:use [clojure.data.csv :as csv]
        [clojure.java.io :as io]
        [sudoku.board :only [read-board, show-board-pretty, show-board-csv]]
        [sudoku.types :only [show-value]]
        [sudoku.strategy :only [solve]]))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "Bad parameters, use <-b> <String>"))
  ([ignore] (println "Bad parameters, use <-b> <String>"))
  ([f1 in] (cond
            (= f1 "-b") (do
                          (println (show-board-pretty (read-board in)))
                          (println (show-board-pretty (solve (read-board in)))))
            (= f1 "-csv") (with-open [in-file (io/reader in)]
                            (let [board (apply str (apply concat (csv/read-csv in-file)))]
                              (println (show-board-csv (solve (read-board board))))))))
  ([f2 f1 in] (cond
               (and (= f1 "-p") (= f2 "-csv")) (with-open [in-file (io/reader in)]
                                                 (let [board (apply str (apply concat (csv/read-csv in-file)))]
                              (println (show-board-pretty (read-board board)))
                              (println (show-board-pretty (solve (read-board board)))))))))


(def test-board-1 (read-board "003020600900305001001806400008102900700000008006708200002609500800203009005010300"))
(def test-board-2 (read-board "200080300060070084030500209000105408000000000402706000301007040720040060004010003"))
