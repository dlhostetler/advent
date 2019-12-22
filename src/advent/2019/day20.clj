(ns advent.2019.day20
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]))

(defn file->grid [file]
  (loop [grid {}
         lines (->> file io/reader line-seq)
         y (dec (count lines))]
    (if-not (empty? lines)
      (recur (->> lines
                  first
                  (map str)
                  (map-indexed (fn [i c] [i c]))
                  (reduce (fn [g [x c]] (assoc g [x y] c)) grid))
             (rest lines)
             (dec y))
      grid)))

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 inc))

(defn south [position]
  (update position 1 dec))

(defn west [position]
  (update position 0 dec))

(defn point->neighbors [point]
  [(east point)
   (north point)
   (south point)
   (west point)])

(defn point->dot-neighbors [grid point]
  (->> (point->neighbors point)
       (filter (comp #(= "." %) grid))))

(defn point->edges [grid edges [point tile]]
  (if (= tile ".")
    (into edges (for [neighbor (point->dot-neighbors grid point)]
                  [point neighbor]))
    edges))

(defn grid->graph [grid all-labels]
  (let [normal-edges (reduce #(point->edges grid %1 %2) #{} grid)
        portal-edges (-> all-labels
                         (dissoc "AA" "ZZ")
                         vals)]
    (apply graph/graph (concat normal-edges portal-edges))))

(defn label-part? [tile]
  (when tile (re-matches #"[A-Z]" tile)))

(defn grid->label-part [grid position]
  (let [tile (get grid position)]
    (when (label-part? tile)
      position)))

(defn position-into-labels [grid labels [position tile]]
  (if-let [other-position (when (label-part? tile)
                            (or (grid->label-part grid (east position))
                                (grid->label-part grid (south position))))]
    (update labels
            (apply str tile (get grid other-position))
            (fnil conj [])
            (or (first (point->dot-neighbors grid position))
                (first (point->dot-neighbors grid other-position))))
    labels))

(defn grid->labels [grid]
  (reduce #(position-into-labels grid %1 %2) {} grid))

(defn run []
  (let [grid (file->grid "resources/day20.input")
        all-labels (grid->labels grid)
        graph (grid->graph grid all-labels)]
    (println "Labels" all-labels)
    (->> (graph.alg/bf-path graph
                            (first (get all-labels "AA"))
                            (first (get all-labels "ZZ")))
         count
         dec)))