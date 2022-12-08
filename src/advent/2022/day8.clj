(ns advent.2022.day8
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def trees
  (->> "resources/2022/day8.input"
       grid/slurp
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(def max-x (grid/grid->max-x trees))
(def max-y (grid/grid->max-y trees))

(defn parse [s]
  s)

(defn edge? [[x y]]
  (or (= x 0)
      (= x max-x)
      (= y 0)
      (= y max-y)))

(defn west-points [[from-x from-y]]
  (for [x (range 0 from-x)]
    [x from-y]))

(defn north-points [[from-x from-y]]
  (for [y (range 0 from-y)]
    [from-x y]))

(defn east-points [[from-x from-y]]
  (for [x (range (inc from-x) (inc max-x))]
    [x from-y]))

(defn south-points [[from-x from-y]]
  (for [y (range (inc from-y) (inc max-y))]
    [from-x y]))

(defn all-lower-than? [trees points height]
  (->> points
       (map trees)
       (every? #(< % height))))

(defn visible? [trees [point height]]
  (if (edge? point)
    true
    (or (all-lower-than? trees (west-points point) height)
        (all-lower-than? trees (north-points point) height)
        (all-lower-than? trees (east-points point) height)
        (all-lower-than? trees (south-points point) height))))

(defn run []
  (->> trees
       (filter (partial visible? trees))
       count))
