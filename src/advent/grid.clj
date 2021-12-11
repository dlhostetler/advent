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

(defn slurp [resource]
  (->> resource
       io/reader
       line-seq
       (mapv vec)
       by-coordinates))


;; Neighbors
;; =========

(defn- max-x [grid]
  (->> grid keys (map first) (apply max)))

(alter-var-root #'max-x memoize)

(defn- max-y [grid]
  (->> grid keys (map last) (apply max)))

(alter-var-root #'max-y memoize)

(defn- valid-x? [grid x]
  (and (>= x 0) (<= x (max-x grid))))

(alter-var-root #'valid-x? memoize)

(defn- valid-y? [grid y]
  (and (>= y 0) (<= y (max-y grid))))

(alter-var-root #'valid-y? memoize)

(defn- neighbor-point [grid [from-x from-y] [x-offset y-offset]]
  (loop [x (+ from-x x-offset)
         y (+ from-y y-offset)]
    (when (and (valid-x? grid x) (valid-y? grid y))
      [x y])))

(defn eight-neighbors [grid point]
  (->> (-> (combo/cartesian-product [-1 0 1] [-1 0 1])
           set
           (disj [0 0]))
       (map #(neighbor-point grid point %))
       (filter some?)))

(alter-var-root #'eight-neighbors memoize)

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

(defn- grid->max-y [grid]
  (bound grid second max))

(defn- grid->min-y [grid]
  (bound grid second min))

(defn- grid->max-x [grid]
  (bound grid first max))

(defn- grid->min-x [grid]
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
     (println))))
