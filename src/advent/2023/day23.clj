(ns advent.2023.day23
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [loom.graph :as graph]
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
  (->> [(grid/north point)
        (grid/east point)
        (grid/south point)
        (grid/west point)]
       (filter #(trail %))))

(defn ->edges [[from tile]]
  (for [p (->neighbors from tile)]
    [from p]))

(def edges
  (->> trail
       (mapcat ->edges)
       (into [])))

(def trailg (apply graph/digraph edges))

(defn intersection? [point]
  (> (count (graph/successors trailg point)) 2))

(def start-end-intersections
  (->> trail
       keys
       (filter intersection?)
       (into #{start end})))

(defn point-to-next-intersection
  [point visited]
  (let [point' (->> (graph/successors trailg point)
                    (remove visited)
                    first)]
    (when (nil? point')
      (throw (Exception. (str "no unvisited successors from " point))))
    (if (start-end-intersections point')
      [point' (count visited)]
      (recur point' (conj visited point')))))

(defn point->intersections [point]
  (->> (graph/successors trailg point)
       (map #(point-to-next-intersection % #{point %}))))

(defn ->intersection-edges [intersection-point]
  (for [[p distance] (point->intersections intersection-point)]
    [intersection-point p distance]))

(def intersection-edges
  (->> start-end-intersections
       (mapcat ->intersection-edges)
       (into [])))

(def intersectionsg (apply graph/weighted-digraph intersection-edges))

(defn done? [paths]
  (->> paths
       (map last)
       (every? #(= end %))))

(defn advance-path [path]
  (if (= (last path) end)
    [path]
    (for [p (graph/successors intersectionsg (last path))
          :when (not (some #(= p %) path))]
      (conj path p))))

(defn find-hike [paths]
  (if (done? paths)
    paths
    (recur (->> paths (mapcat advance-path) (remove nil?) doall))))

(defn path->length [path]
  (->> path
       (partition 2 1)
       (map #(apply graph/weight intersectionsg %))
       (reduce +)))

(defn run []
  (->> (find-hike [[start]])
       (map path->length)
       (reduce max)))
