(defproject advent "1.0.0-SNAPSHOT"
  :description "DH's solutions for the Advent of Code."
  :url "https://github.com/d-hostetler/advent"
  :dependencies [[aysylu/loom "1.0.2"]
                 [instaparse "1.4.10"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.flatland/ordered "1.15.11"]
                 [prismatic/plumbing "0.5.5"]]
  :profiles {:uberjar {:aot :all}})
