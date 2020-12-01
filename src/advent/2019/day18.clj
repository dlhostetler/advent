(ns advent.2019.day18
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]
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

(defn point->edges [grid edges [point tile]]
  (if (not= tile "#")
    (let [neighbors (->> [(east point)
                          (north point)
                          (south point)
                          (west point)]
                         (remove (comp #(= "#" %) grid))
                         (remove (comp nil? grid)))]
      (into edges (for [neighbor neighbors]
                    [point neighbor])))
    edges))

(defn grid->graph [grid]
  (apply graph/graph (reduce #(point->edges grid %1 %2) #{} grid)))

(defn key? [tile]
  (re-matches #"[a-z]" tile))

(defn grid-key? [[position tile]]
  (key? tile))

(defn door? [tile]
  (re-matches #"[A-Z]" tile))

(defn grid-door? [[position tile]]
  (door? tile))

(defn door->key [tile]
  (if (door? tile)
    (str/lower-case tile)
    tile))

(defn path->cost [path]
  (-> path count dec))


(defn all-keys [grid]
  (->> grid
       vals
       (filter key?)
       (into #{})))

(defn viable-path? [grid have-ks [tile path]]
  (when path
    (let [need-ks (->> path
                       (map grid)
                       (filter door?)
                       (map door->key)
                       (into #{}))]
      (empty? (set/difference need-ks have-ks)))))

(defn poi-to-poi-path [paths graph tile->position poi0 poi1]
  (assoc-in paths
            [poi0 poi1]
            (graph.alg/bf-path graph
                               (tile->position poi0)
                               (tile->position poi1))))

(defn poi-paths [paths graph tile->positions all-pois poi0]
  (reduce #(poi-to-poi-path %1 graph tile->positions poi0 %2)
          paths
          all-pois))

(defn all-poi-paths [grid graph tile->positions]
  (let [all-pois (-> grid all-keys (conj "1" "2" "3" "4"))]
    (reduce #(poi-paths %1 graph tile->positions all-pois %2) {} all-pois)))

(def ^:private inf Double/POSITIVE_INFINITY)

(defn available-keys [grid poi-paths all-ks all-current need-ks]
  (let [have-ks (set/difference all-ks need-ks)]
    (for [current all-current
          :let [potential-paths (-> (get poi-paths current)
                                    (select-keys need-ks))]
          k (->> potential-paths
                 (filter #(viable-path? grid have-ks %))
                 keys)]
      [current k])))

(defn cost [poi-paths current k]
  (path->cost (get-in poi-paths [current k])))

(defn cost-for-keys [grid poi-paths tile->position all-ks all-current need-ks]
  (if (empty? need-ks)
    0
    (loop [current-k-pairs (available-keys grid poi-paths all-ks all-current need-ks)
           result inf]
      (if-not (empty? current-k-pairs)
        (let [[current k] (first current-k-pairs)
              cost-to-k (cost poi-paths current k)
              cost-through-k (cost-for-keys grid
                                            poi-paths
                                            tile->position
                                            all-ks
                                            (-> all-current
                                                (disj current)
                                                (conj k))
                                            (disj need-ks k))
              total-cost (+ cost-to-k cost-through-k)]
          (recur (rest current-k-pairs) (min result total-cost)))
        result))))

(alter-var-root #'cost-for-keys memoize)

(defn run []
  (let [grid (file->grid "resources/2019/day18.input")
        tile->position (set/map-invert grid)
        graph (grid->graph grid)
        poi-paths (all-poi-paths grid graph tile->position)]
    (cost-for-keys grid poi-paths tile->position (all-keys grid) #{"1" "2" "3" "4"} (all-keys grid))))
