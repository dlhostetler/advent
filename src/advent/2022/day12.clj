(ns advent.2022.day12
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]
            [plumbing.core :refer :all]))

(def heights
  (->> "resources/2022/day12.input"
       io/reader
       grid/slurp
       (map-vals str)
       (map-vals keyword)))

(defn find-coords [target]
  (->> heights
       (filter (fn [[_ v]] (= v target)))
       first
       first))

(def start-at (find-coords :S))

(def end-at (find-coords :E))

(defn height-value [v]
  (cond
    (= v :S)
    0
    (= v :E)
    25
    :else
    (- (-> v name first int) (int \a))))

(defn valid-destination? [heights from to]
  (let [from-n (-> from heights height-value)
        to-n (-> to heights height-value)
        diff (- to-n from-n)]
    (<= diff 1)))

(defn edges [heights [from _]]
  (for [edge (->> from
                  (grid/cardinal-neighbors heights)
                  (filter #(valid-destination? heights from %)))]
    [from edge]))

(defn run []
  (let [edges (mapcat (partial edges heights) heights)
        g (apply graph/digraph edges)]
    (->> (graph.alg/bf-path g start-at end-at)
         count
         dec)))
