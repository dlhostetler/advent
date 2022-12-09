(ns advent.2022.day8
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [plumbing.core :refer :all]))

(def trees
  (->> "resources/2022/day8.input"
       grid/slurp
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(def max-x (grid/grid->max-x trees))
(def max-y (grid/grid->max-y trees))

(defn count-lower-than? [trees points height]
  (->> points
       (map trees)
       (seq/take-while+1 #(< % height))
       count))

(defn scenic-score [trees [point height]]
  (* (count-lower-than? trees (grid/points-west trees point) height)
     (count-lower-than? trees (grid/points-north trees point) height)
     (count-lower-than? trees (grid/points-east trees point) height)
     (count-lower-than? trees (grid/points-south trees point) height)))

(defn run []
  (->> trees
       (map (partial scenic-score trees))
       (reduce max)))
