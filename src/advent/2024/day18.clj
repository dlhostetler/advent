(ns advent.2024.day18
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]))

(def max-x 70)
(def max-y 70)

(defn parse-coord [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(def input
  (->> (-> "resources/2024/day18.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-coord)))

(def space-points
  (into []
        (for [x (range (inc max-x))
              y (range (inc max-x))]
          [x y])))

(defn in-bounds? [[x y]]
  (and (<= 0 x max-x) (<= 0 y max-y)))

(defn neighbors [p]
  (->> [(grid/north p)
        (grid/east p)
        (grid/south p)
        (grid/west p)]
       (filter in-bounds?)))

(defn run []
  (loop [graph (graph/digraph (zipmap space-points (map neighbors space-points)))
         bytes input]
    (let [byte (first bytes)
          next-graph (graph/remove-nodes graph byte)]
      (if (graph.alg/bf-path next-graph [0 0] [max-x max-y])
        (recur next-graph (rest bytes))
        byte))))
