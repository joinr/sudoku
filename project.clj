(defproject sudoku "0.1.0-SNAPSHOT"
  :description "Exploration of Dr. Norvig's sudoku solver"
  :url "https://github.com/cnuernber/sudoku"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [techascent/tech.datatype "4.66"]
                 [structural "0.1.2-SNAPSHOT"]]
  :repl-options {:init-ns sudoku.longboard}
  :main sudoku.longboard
  :profiles {:uberjar {:aot [sudoku.longboard]
                       :uberjar-name "sudoku.jar"}})
