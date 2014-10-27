(ns sudoku.board
  (:use [clojure.set :only [difference, union]]
        [sudoku.types :only [options-from-int, show-value, cells, all-houses]]))

(def ^:private pretty-board
  (apply str
      ["‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒\n"
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
       "‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒\n",
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
       "‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒‒+‒‒‒‒‒‒‒‒‒‒‒‒\n",
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
          "%s %s %s | %s %s %s | %s %s %s \n"
       "‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒‒\n"]))

(defn ^:private read-board-string [s]
  "Read the board from a String"
  (apply vector
         (map options-from-int
              (map (fn [c] (- (int c) (int \0)))
                   (seq s)))))

(defn read-board [s]
  "Read a board from the string, returning it in a map"
  {:Status :New :Board (read-board-string s)})

(defn show-board-pretty [{b :Board}]
  "Return a string with the formatted board"
  (apply format pretty-board (map show-value b)))

(defn- in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn neighbors? [c1 c2]
  "Return true if two cells share a house"
  (and (not= c1 c2)
       (> (count (filter true? (map (fn [h] (and (in? h c1) (in? h c2))) all-houses))) 0)))

(defn ops-to-remove [c rms]
  "Return a set of optios to remove, by collecting the removals neighboring c"
  (reduce union #{} (map (fn [[p v]] v) (filter (fn [[p v]] (neighbors? c p)) rms) )))

(defn remove-all-if-neighbor [rms opss c]
  "Remove multiple ops from each opss that is neighboor of cell"
  (difference (nth opss c) (ops-to-remove c rms)))

(defn simplify [rms opps]
  "Remove the options if the cell is neighbouring cell"
  (mapv (fn [c] (remove-all-if-neighbor rms opps c)) cells))

(defn solution? [opss]
  "If all the cells have a single value assigned, the board is solved"
  (every? (fn [ops] (= 1 (count ops))) opss))

(defn unsolvable? [opss]
  "If any of the cells is empty, the board can't be solved"
  (some (fn [ops] (= 0 (count ops))) opss))

(defn solvable? [opss]
  "If any of the cells is empty, the board can't be solved"
  ((comp not unsolvable?) opss))

(defn simplify-board [{opss :Board} rms]
  "Remove some values from cell"
  (let [new-opss (simplify rms opss)]
    (cond (unsolvable? new-opss) {:Status :Unsolvable :Board new-opss}
          (solution? new-opss)   {:Status :Solved     :Board new-opss}
          :else                  {:Status :Solvable   :Board new-opss})))

;; Board -> Board
(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn pos-to-ops [pos coll]
  (mapv hash-set (into [] (nth coll pos))))

(defn is-expandable [ops]
  (> (count ops) 1))

(defn expand-board [board]
  ""
  (let [opss  (:Board board)
        pos   (first-index-of is-expandable opss)
        pre   (take (+ pos 1) opss)
        suf   (drop (+ pos 2) opss)
        exp   (pos-to-ops pos opss)]
    (mapv (fn [v] {:Status :New :Board (apply vector (concat pre [v] suf))}) exp)))
