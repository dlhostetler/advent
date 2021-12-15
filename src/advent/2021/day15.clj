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

(defn run []
  (let [wg (->> point->risk
                keys
                (mapcat #(point->edges point->risk %))
                (apply graph/weighted-digraph))
        max-x (grid/max-x point->risk)
        max-y (grid/max-y point->risk)
        [_ cost] (graph.alg/dijkstra-path-dist wg [0 0] [max-x max-y])]
    cost))
