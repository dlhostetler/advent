(ns advent.2019.day20
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

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

(defn point->edges [grid edges [point tile] level]
  (if (= tile ".")
    (into edges (for [neighbor (point->dot-neighbors grid point)]
                  [(conj point level) (conj neighbor level)]))
    edges))

(defn inner-portal-edges [all-labels level]
  (for [[outer inner] (-> all-labels (dissoc "AA" "ZZ") vals)]
    [(conj inner level) (conj outer (inc level))]))

(defn outer-portal-edges [all-labels level]
  (for [[outer inner] (-> all-labels (dissoc "AA" "ZZ") vals)]
    [(conj outer level) (conj inner (dec level))]))

(defn grid->edges
  [grid all-labels level]
  (let [edges (concat (reduce #(point->edges grid %1 %2 level) #{} grid)
                      (inner-portal-edges all-labels level))]
    (if (pos? level)
      (concat edges (outer-portal-edges all-labels level))
      edges)))

(defn grid->graph [grid all-labels levels]
  (let [edges (into [] (mapcat #(grid->edges grid all-labels %) (range levels)))]
    ;(clojure.pprint/pprint edges)
    (apply graph/graph edges)))

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

(defn outer? [west-x east-x north-y south-y [x y]]
  (or (= west-x x)
      (= east-x x)
      (= north-y y)
      (= south-y y)))

(defn order-label-positions
  [west-x east-x north-y south-y [position0 position1 :as positions]]
  (if (and position0 position1)
    (if (outer? west-x east-x north-y south-y position0)
      [position0 position1]
      [position1 position0])
    ;; AA or ZZ
    positions))

(defn grid->labels [grid]
  (let [west-x 2
        east-x (- (->> grid
                       keys
                       (map first)
                       (apply max)) 2)
        north-y (- (->> grid
                        keys
                        (map last)
                        (apply max)) 2)
        south-y 2]
    (->> grid
         (reduce #(position-into-labels grid %1 %2) {})
         (map-vals #(order-label-positions west-x east-x north-y south-y %)))))

(defn run []
  (let [grid (file->grid "resources/2019/day20.input")
        all-labels (grid->labels grid)
        graph (grid->graph grid all-labels 50)]
    ;(println "Labels" all-labels)
    (->> (graph.alg/bf-path graph
                            (conj (first (get all-labels "AA")) 0)
                            (conj (first (get all-labels "ZZ")) 0))
           count
           dec)))