(ns advent.2023.day23
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

(def trail
  (->> (-> "resources/2023/day23.input"
           io/reader
           grid/slurp)
       (filter (comp #(not= \# %) last))
       (into {})))

(def start (->> trail
                keys
                (filter (comp (partial = 0) grid/y))
                first))

(def end (->> trail
              keys
              (filter (comp (partial = (grid/max-y trail)) grid/y))
              first))

(defn ->neighbors [point tile]
  (->> (case tile
         \^
         [(grid/north point)]

         \>
         [(grid/east point)]

         \v
         [(grid/south point)]

         \<
         [(grid/west point)]

         ;; \.
         [(grid/north point)
          (grid/east point)
          (grid/south point)
          (grid/west point)])
       (filter #(trail %))))

(defn ->edges [[from tile]]
  (for [p (->neighbors from tile)]
    [from p]))

(def edges
  (->> trail
       (mapcat ->edges)
       (into [])))

(def trailg (apply graph/digraph edges))

(defn done? [paths]
  (->> paths
       (map last)
       (every? #(= end %))))

(defn advance-path [path]
  (if (= (last path) end)
    [path]
    (for [p (graph/successors trailg (last path))
          :when (not (some #(= p %) path))]
      (conj path p))))

(defn find-hike [paths]
  (if (done? paths)
    paths
    (recur (->> paths (mapcat advance-path) (remove nil?) doall))))

(defn run []
  (->> (find-hike [[start]])
       (map count)
       (map dec)
       (reduce max)))
