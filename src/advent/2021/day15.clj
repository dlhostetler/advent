(ns advent.2021.day15
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

(def point->risk
  (->> "resources/2021/day15.input"
       io/reader
       grid/slurp
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(defn edge [grid from to]
  [from to (get grid to)])

(defn point->edges [grid point]
  (map edge
       (repeat grid)
       (repeat point)
       (grid/cardinal-neighbors grid point)))

(defn clamp-risk [risk]
  (if (> risk 9)
    (mod risk 9)
    risk))

(defn explode [max-x max-y [[x y] risk]]
  (let [next-x (inc max-x)
        next-y (inc max-y)]
    (for [repeat-x (range 5)
          repeat-y (range 5)
          :let [plus-x (* repeat-x next-x)
                plus-y (* repeat-y next-y)]]
      [[(+ x plus-x) (+ y plus-y)]
       (clamp-risk (+ risk repeat-x repeat-y))])))

(defn run []
  (let [full-point->risk (->> point->risk
                              (mapcat #(explode (grid/max-x point->risk)
                                                (grid/max-y point->risk)
                                                %))
                              (into {}))
        wg (->> full-point->risk
                keys
                (mapcat #(point->edges full-point->risk %))
                (apply graph/weighted-digraph))
        max-x (grid/max-x full-point->risk)
        max-y (grid/max-y full-point->risk)
        [_ cost] (graph.alg/dijkstra-path-dist wg [0 0] [max-x max-y])]
    cost))
