(ns sudoku.types
  (:use [clojure.set :only [difference]]
        ))

(def cells (apply vector (take 81 (range))))

(def possible [:one :two :three :four :five
               :six :seven :eight :nine])

(def all (apply hash-set possible))

(defn show-value [v]
  "Convert a vector of values to a sintr representation"
  (cond
   (= 0 (count v)) " ⊥ "
   (= 1 (count v)) (str " " (+ 1 (.indexOf possible (first v))) " ")
   :else " • "
   ))

(defn show-value-csv [v]
  "Convert a vector of values to a sintr representation"
  (cond
   (= 0 (count v)) "⊥"
   (= 1 (count v)) (str (+ 1 (.indexOf possible (first v))))
   :else "•"
   ))

(defn options-from-int [i]
  "Return a vector of possible values"
  (if (= 0 i)
      all
      #{(possible (- i 1))}))

(defn assigned? [ops]
  "Return true if the options set has only one member"
  (= 1 (count ops)))

(defn expandable? [ops]
  "Return true if the options set has only one member"
  (> 1 (count ops)))

(defn expand [ops]
  "Expand a set of options into a vector of single options"
  (map (fn [x] [#{x}]) ops))

(def row-starts (apply vector (range 0 81 9)))
(def all-rows (apply vector
                     (map (fn [n] (apply vector (range n (+ n 9))))
                          row-starts)))

(def col-starts (apply vector (range 0 9)))
(def all-cols (apply vector
                     (map (fn [n] (apply vector (range n (+ n 81) 9)))
                          col-starts)))

(def box1 [ 0  1  2
            9 10 11
           18 19 20])

(def all-boxes (apply vector
                      (map (fn [n] (apply vector (map (fn [v] (+ v n)) box1)))
                           [0 3 6 27 30 33 54 57 60])))

(def all-houses (concat all-rows all-cols all-boxes))
