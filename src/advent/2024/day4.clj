(ns advent.2024.day4
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def input
  (->> "resources/2024/day4.input"
       grid/slurp
       (map-vals str)
       (remove (comp (partial = ".") last))
       (into {})))

(defn nw-to-se-word [p]
  (str (input (grid/northwest p))
       (input p)
       (input (grid/southeast p))))

(defn ne-to-sw-word [p]
  (str (input (grid/northeast p))
       (input p)
       (input (grid/southwest p))))

(defn x-mas? [[x y]]
  (and (or (= (nw-to-se-word [x y]) "MAS")
           (= (nw-to-se-word [x y]) "SAM"))
       (or (= (ne-to-sw-word [x y]) "MAS")
           (= (ne-to-sw-word [x y]) "SAM"))))

(defn run []
  (->> input
       keys
       (map x-mas?)
       (filter true?)
       count))
