(ns sudoku.strategy
  (:use [sudoku.types :only [cells]]
        [sudoku.board :only [simplify-board, expand-board, solvable? ,unsolvable?, solution?]]))

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
  (let [b (expand-board board)]
    (if (seq? b)
      (filter solvable? (map (fn [b'] (apply-strategy naked-single b')) b))
      b)))

;; [Board] -> [Board]
(defn disp
  ""
  ([] :Empty)
  ([b & bs] (if (nil? b) :Skip (:Status b))))

(defmulti search disp)
(defmethod search :Empty [] {:Status :Unsolvable} )
(defmethod search :Skip [b & bs] (apply search bs))
(defmethod search :Solved [b & bs] b)
(defmethod search :Unsolvable [b & bs] (apply search bs))
(defmethod search :Unsolvable [b & bs] (apply search bs))
(defmethod search :New [b & bs]  (apply search (apply-strategy naked-single b) bs))

(defmethod search :Solvable [b & bs]
  (let [b' (expand-and-reduce b)
        bs' (concat b' bs)]
    (apply search bs')))

(defn solve [board]
  "Solve a sudoku board and returned wrapped in a map"
  (search board))
