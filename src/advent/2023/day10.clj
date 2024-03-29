(ns advent.2023.day10
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

(def wall-tiles #{"L", "J", "|", "S"})

(defn empty-tile? [[_ tile]]
  (= "." tile))

(def all-tiles
  (->> (-> "resources/2023/day10.input"
           io/reader
           grid/slurp)
       (map-vals str)
       (into {})))

(def tiles
  (->> all-tiles
       (remove empty-tile?)
       (into {})))

(def s-point
  (->> tiles
       (filter (comp #(= "S" %) val))
       first
       first))

(defn points->edges [from-point to-points]
  (into []
        (for [p (filter tiles to-points)]
          [from-point p])))

(defmulti ->edges
  (fn [[point tile]]
    tile))

(defmethod ->edges "|" [[point]]
  (points->edges point [(grid/north point) (grid/south point)]))

(defmethod ->edges "-" [[point]]
  (points->edges point [(grid/east point) (grid/west point)]))

(defmethod ->edges "L" [[point]]
  (points->edges point [(grid/north point) (grid/east point)]))

(defmethod ->edges "J" [[point]]
  (points->edges point [(grid/north point) (grid/west point)]))

(defmethod ->edges "7" [[point]]
  (points->edges point [(grid/south point) (grid/west point)]))

(defmethod ->edges "F" [[point]]
  (points->edges point [(grid/east point) (grid/south point)]))

(defmethod ->edges "S" [[point]]
  [])

(defn with-s-edges [edges]
  (let [from-points (->> edges
                         (filter (comp #(= s-point %) last)))]
    (when (not= 2 (count from-points))
      (throw (Exception. (str "there were " (count from-points) " s edges"))))
    (->> (for [p (map first from-points)]
           [s-point p])
         (into [])
         (concat edges))))

(defn num-west-walls [loop-points [x y]]
  (->> loop-points
       (filter (fn [[loop-point-x loop-point-y]]
                 (and (= loop-point-y y)
                      (< loop-point-x x))))
       (map tiles)
       (filter wall-tiles)
       count))

(defn inside? [loop-points point]
  (num-west-walls loop-points point)
  (odd? (num-west-walls loop-points point)))

(defn run []
  (let [edges (->> tiles
                   (mapcat ->edges)
                   with-s-edges)
        g (apply graph/digraph edges)
        loop-points (->> (graph.alg/scc g)
                         (map set)
                         (filter #(% s-point))
                         first)]
    (->> all-tiles
         keys
         (remove loop-points)
         (filter (partial inside? loop-points))
         count)))
