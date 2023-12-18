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

;; Point(s)
;; ========

(defn x [point]
  (first point))

(defn y [point]
  (second point))

(defn pred-point->tile= [grid target-tile]
  (fn [point]
    (= (get grid point) target-tile)))

(defn pred-tile= [target-tile]
  (fn [[_ tile]]
    (= tile target-tile)))

(defn with-tiles
  ([grid points]
   (with-tiles grid points nil))
  ([grid points not-found]
   (map (fn [point]
          [point (get grid point not-found)])
        points)))

;; Edges
;; =====

(defn max-x [grid]
  (->> grid keys (map first) (apply max)))

(alter-var-root #'max-x memoize)

(defn max-x+1 [grid]
  (inc (max-x grid)))

(alter-var-root #'max-x+1 memoize)

(defn max-y [grid]
  (->> grid keys (map last) (apply max)))

(alter-var-root #'max-y memoize)

(defn max-y+1 [grid]
  (inc (max-y grid)))

(alter-var-root #'max-y+1 memoize)

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

(defn valid-point-or-nil [grid [x y]]
  (when (and (valid-x? grid x) (valid-y? grid y))
    [x y]))

(alter-var-root #'valid-point-or-nil memoize)

;; Col/Row
;; =======

(defn points-col [grid x]
  (for [y (range (min-y grid) (inc (max-y grid)))
        :let [point [x y]]
        :when (get grid point)]
    point))

(defn points-row [grid y]
  (for [x (range (min-x grid) (inc (max-x grid)))
        :let [point [x y]]
        :when (get grid point)]
    point))

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

(defn rotate-90-neg [grid]
  (map-keys (fn [[x y]]
              [y (* -1 x)])
            grid))

(defn rotate-90-pos [grid]
  (map-keys (fn [[x y]]
              [(* -1 y) x])
            grid))

(defn rotate-180 [grid]
  (map-keys (fn [[x y]]
              [(* -1 x) (* -1 y)])
            grid))

;; Flood
;; =====

(defn- points-to-flood [grid flooded? last-flooded neighbors-fn should-fill?]
  (set
    (for [point (mapcat neighbors-fn (repeat grid) last-flooded)
          :when (and (not (flooded? point))
                     (should-fill? (get grid point)))]
      point)))

(defn flood [grid {:keys [fill-with
                          max-steps
                          neighbors-fn
                          should-fill?
                          start-point]
                   :or {fill-with \*
                        max-steps 10000
                        neighbors-fn cardinal-neighbors
                        should-fill? (fn [tile] (or (nil? tile) (= tile \.)))
                        start-point [0 0]}}]
  (loop [flooded #{start-point}
         last-flooded #{start-point}
         steps 0]
    (when (> steps max-steps)
      (throw (Exception. (str "reached " max-steps " with no solution"))))
    (let [to-flood (points-to-flood grid
                                    flooded
                                    last-flooded
                                    neighbors-fn
                                    should-fill?)
          flooded' (into flooded to-flood)]
      (if (not= flooded flooded')
        (recur flooded' to-flood (inc steps))
        {:grid (->> flooded
                    (map (fn [point] [point fill-with]))
                    (into grid))
         :steps steps}))))

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
  ([grid]
   (print grid identity))
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
                               padding 0
                               y-dir :top-down}}]
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

(defn print->
  ([grid]
   (print-> grid identity))
  ([grid visualize-point]
   (print-> grid visualize-point {}))
  ([grid visualize-point options]
   (print grid visualize-point options)
   grid))