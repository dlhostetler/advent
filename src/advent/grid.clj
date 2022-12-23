(ns advent.grid
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all])
  (:refer-clojure :rename {print core-print
                           slurp core-slurp}))

;; IO
;; ==

(defn- by-coordinates [grid]
  (->> (for [y (range (count grid))
             x (range (-> grid first count))]
         [[x y] (-> grid
                    (nth y)
                    (nth x))])
       (into {})))

(defn lines->grid [lines]
  (->> lines
       (mapv vec)
       by-coordinates))

(defn slurp [resource]
  (->> resource
       io/reader
       line-seq
       lines->grid))

;; Edges
;; =====

(defn max-x [grid]
  (->> grid keys (map first) (apply max)))

(alter-var-root #'max-x memoize)

(defn max-y [grid]
  (->> grid keys (map last) (apply max)))

(alter-var-root #'max-y memoize)

(defn min-x [grid]
  (->> grid keys (map first) (apply min)))

(alter-var-root #'min-x memoize)

(defn min-y [grid]
  (->> grid keys (map last) (apply min)))

(alter-var-root #'min-y memoize)

(defn valid-x? [grid x]
  (and (>= x (min-y grid)) (<= x (max-x grid))))

(alter-var-root #'valid-x? memoize)

(defn valid-y? [grid y]
  (and (>= y (min-y grid)) (<= y (max-y grid))))

(alter-var-root #'valid-y? memoize)

(defn- valid-point-or-nil [grid [x y]]
  (when (and (valid-x? grid x) (valid-y? grid y))
    [x y]))

;; Neighbors
;; =========

(defn- neighbor-point [grid [from-x from-y] [x-offset y-offset]]
  (loop [x (+ from-x x-offset)
         y (+ from-y y-offset)]
    (valid-point-or-nil grid [x y])))

(defn eight-neighbors [grid point]
  (->> (-> (combo/cartesian-product [-1 0 1] [-1 0 1])
           set
           (disj [0 0]))
       (map #(neighbor-point grid point %))
       (filter some?)))

(alter-var-root #'eight-neighbors memoize)

(defn east [point]
  (when point (update point 0 inc)))

(defn north [point]
  (when point (update point 1 dec)))

(defn northeast [point]
  (when point (-> point (update 0 inc) (update 1 dec))))

(defn northwest [point]
  (when point (-> point (update 0 dec) (update 1 dec))))

(defn south [point]
  (when point (update point 1 inc)))

(defn southeast [point]
  (when point (-> point (update 0 inc) (update 1 inc))))

(defn southwest [point]
  (when point (-> point (update 0 dec) (update 1 inc))))

(defn west [point]
  (when point (update point 0 dec)))

(defn cardinal-neighbors [grid point]
  (->> [(north point)
        (east point)
        (south point)
        (west point)]
       (map #(valid-point-or-nil grid %))
       (filter some?)))

(defn points-east [grid [from-x from-y]]
  (for [x (range (inc from-x) (-> grid max-x inc))]
    [x from-y]))

(defn points-north [grid [from-x from-y]]
  (-> (for [y (range (min-y grid) from-y)]
        [from-x y])
      reverse))

(defn points-south [grid [from-x from-y]]
  (for [y (range (inc from-y) (-> grid max-y inc))]
    [from-x y]))

(defn points-west [grid [from-x from-y]]
  (-> (for [x (range (min-x grid) from-x)]
        [x from-y])
      reverse))

(alter-var-root #'cardinal-neighbors memoize)

;; Transformation
;; ==============

(defn tx-points [tx-point starting-grid]
  (loop [grid starting-grid
         to-tx (keys starting-grid)
         txed? #{}]
    (if-not (empty? to-tx)
      (let [point (first to-tx)
            {:keys [also-tx revisit? value]} (tx-point point {:grid grid
                                                              :txed? txed?})]
        (recur (assoc grid point value)
               (-> to-tx
                   rest
                   (?> also-tx (into also-tx)))
               (cond-> txed?
                       (not revisit?) (conj point))))
      (into {} grid))))

;; Visualization
;; =============

(defn- bound [grid dimension-fn agg-fn]
  (->> grid
       keys
       (map dimension-fn)
       (apply agg-fn)))

(defn grid->max-y [grid]
  (bound grid second max))

(defn grid->min-y [grid]
  (bound grid second min))

(defn grid->max-x [grid]
  (bound grid first max))

(defn grid->min-x [grid]
  (bound grid first min))

(defn print
  ([grid visualize-point]
   (print grid visualize-point {}))
  ([grid visualize-point {:keys [default
                                 empty-point
                                 max-x
                                 max-y
                                 min-x
                                 min-y
                                 padding
                                 y-dir]
                          :or {empty-point "."
                               padding 1}}]
   (when-not (empty? grid)
     (doseq [y (cond-> (range (- (or min-y (grid->min-y grid)) padding)
                              (inc (+ (or max-y (grid->max-y grid)) padding)))
                       (not= :top-down y-dir)
                       reverse)]
       (doseq [x (range (- (or min-x (grid->min-x grid)) padding)
                        (inc (+ (or max-x (grid->max-x grid)) padding)))
               :let [point (get grid [x y] default)]]
         (core-print (if-let [out (when point (visualize-point point))]
                       out
                       empty-point)))
       (println))
     (println))))

(defn print-> [grid visualize-point options]
  (print grid visualize-point options)
  grid)