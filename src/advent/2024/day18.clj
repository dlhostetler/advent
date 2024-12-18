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

(defn neighbors [memory p]
  (->> [(grid/north p)
        (grid/east p)
        (grid/south p)
        (grid/west p)]
       (filter in-bounds?)
       (remove memory)))

(defn run []
  (let [memory (zipmap (set (take 1024 input)) (repeat "#"))
        edges (zipmap space-points
                      (map (partial neighbors memory) space-points))
        graph (graph/digraph edges)]
    (dec (count (graph.alg/bf-path graph [0 0] [max-x max-y])))))
