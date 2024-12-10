(ns advent.2024.day10
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def terrain
  (->> "resources/2024/day10.input"
       grid/slurp
       (remove (comp (partial = \.) val))
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(def starting-positions
  (->> terrain
       (filter (comp (partial = 0) val))
       (map first)))

(defn rating-from [p]
  (let [height (get terrain p)]
    (if (= 9 height)
     1
     (->> (for [neighbor (grid/cardinal-neighbors terrain p)
                :when (= (get terrain neighbor) (inc height))]
            (rating-from neighbor))
          (reduce + 0)))))

(defn run []
  (->> starting-positions
       (map rating-from)
       (reduce +)))
