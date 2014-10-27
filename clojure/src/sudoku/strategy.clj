(ns sudoku.strategy
  (:use [sudoku.types :only [cells]]
        [sudoku.board :only [simplify-board, expand-board, solvable?, solution?]]))

(defn get-cells [pred? opss]
  ""
  (apply vector
         (filter (comp not nil?)
                 (map (fn [n] (let [ops (nth opss n)]
                                (if (pred? ops) [n ops] nil)))
                      cells))))

(defn single? [ops]
  ""
  (= 1 (count ops)))

(defn naked-single [{opss :Board :as board}]
  "If a cell can take only one value, remove that value from all its neighbors"
  (simplify-board board (get-cells single? opss)))

(defn apply-strategy [strategy board]
  ""
  (let [new-board (strategy board)]
    (cond (= (:Status new-board) :Solved)       new-board
          (= (:Board new-board) (:Board board)) new-board
          :else (recur strategy new-board))
    ))

;; Board -> [Board]
(defn expand-and-reduce [board]
  ""
  (filter solvable? (map (fn [b] (apply-strategy naked-single b)) (expand-board board))))

;; [Board] -> [Board]
(defn search [boards]
  ""
  (let [f   (first boards)
        r   (rest  boards)
        nbs (expand-and-reduce f)]
    (println (count (:Board f)))
    (if (solution? f) [f] (recur (concat nbs r)))))

  (defn solve [board]
    "Solve a sudoku board and returned wrapped in a map"
    (search [board]))
