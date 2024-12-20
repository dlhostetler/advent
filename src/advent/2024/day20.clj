(ns advent.2024.day20
  (:require [advent.grid :as grid]
            [clojure.math.combinatorics :as combo]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]
            [plumbing.core :refer :all]))

(def racetrack
  (->> "resources/2024/day20.input"
       grid/slurp
       (map-vals str)))

(def start
  (->> racetrack
       (filter (comp (partial = "S") val))
       first
       first))

(def end
  (->> racetrack
       (filter (comp (partial = "E") val))
       first
       first))

(defn initial-edges []
  (into {}
        (for [[p tile] racetrack
              :when (not= tile "#")]
          [p (into []
                   (for [neighbor (grid/cardinal-neighbors racetrack p)
                         :when (not= (get racetrack neighbor) "#")]
                     neighbor))])))

(defn calc-distances [path]
  (loop [n 0
         path path
         distances {}]
    (if (empty? path)
      distances
      (recur (inc n)
             (rest path)
             (assoc distances (first path) n)))))

(defn manhattan [[x0 y0] [x1 y1]]
  (int (+ (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))))

(defn cheat? [[from to]]
  (let [d (manhattan from to)]
    (<= d 20)))

(defn savings [distances [from to]]
  (- (- (distances to) (distances from)) (manhattan from to)))

(defn run []
  (let [graph (graph/digraph (initial-edges))
        normal-path (graph.alg/bf-path graph start end)
        distances (calc-distances normal-path)]
    (->> (combo/combinations normal-path 2)
         (filter cheat?)
         (map (partial savings distances))
         (filter #(>= % 100))
         count)))
