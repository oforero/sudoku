(ns sudoku.core-test
  (:require [clojure.test :refer :all])
  (:use [sudoku.board :only [read-board]]
        [sudoku.strategy :only [solve]]
        [sudoku.grids]))

(defmulti is-solution :Status)
(defmethod is-solution :Solved [_]
  true)
(defmethod is-solution :default [_]
  false)

(deftest test-solve-easy-grids
  (testing "It should solve all easy grids"
    (is (every? (comp is-solution solve read-board) easy-grids))))

(deftest test-do-not-solve-unsolvable-grids
  (testing "It should not solve any of the unsolvable grids"
    (is (every? (comp not is-solution solve read-board) unsolvable-grids))))
