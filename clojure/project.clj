(defproject sudoku "0.1.0-SNAPSHOT"
  :description "A simple sudoku solver"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://raw.githubusercontent.com/oforero/sudoku/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.6.0"]
		 [org.clojure/data.csv "0.1.2"]]
  :main ^:skip-aot sudoku.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
