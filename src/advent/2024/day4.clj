(ns advent.2024.day4
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def dirs
  [grid/east
   grid/southeast
   grid/south
   grid/southwest
   grid/west
   grid/northwest
   grid/north
   grid/northeast])

(def input
  (->> "resources/2024/day4.input"
       grid/slurp
       (map-vals str)
       (remove (comp (partial = ".") last))
       (into {})))

(defn four-letters-from [p dir]
  (loop [n 4
         p p
         letters ""]
    (if (zero? n)
      letters
      (recur (dec n) (dir p) (str letters (get input p))))))

(defn xmas? [[x y] dir]
  (= (four-letters-from [x y] dir) "XMAS"))

(defn count-xmases-from [p]
  (->> (for [dir dirs] (xmas? p dir))
       (filter true?)
       count))

(defn run []
  (->> input
       keys
       (map count-xmases-from)
       (reduce +)))
