(ns advent.2020.day20
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def tile-size 10)
(def all-txs (combo/cartesian-product [0 90 180 270]
                                      [false true]
                                      [false true]))

(defn parse-tile [lines]
  (let [[header grid] (str/split lines #"\n" 2)
        [tile-id] (rest (re-matches #"Tile (\d+):" header))]
    [(Integer/parseInt tile-id)
     (str/split-lines grid)]))

(defn parse-tiles []
  (->> (-> "resources/2020/day20.input"
           slurp
           (str/split #"\n\n"))
       (map parse-tile)
       (into {})))

(defn negate [n]
  (- tile-size n 1))

(defn flip-dimension [dimension flip?]
  (if flip? (negate dimension) dimension))

;; 90 = (b, -a); 180 = (-a, -b); 270 = (-b, a); 360 = (a, b)
(defn rotate-coords [[x y] degrees]
  (case degrees
    0
    [x y]
    90
    [y (negate x)]
    180
    [(negate x) (negate y)]
    270
    [(negate y) x]))

(defn transform-coords [[x y] [degrees flip-x? flip-y?]]
  (-> [(flip-dimension x flip-x?)
       (flip-dimension y flip-y?)]
      (rotate-coords degrees)))

(defn tile-at [grid coords tx]
  (let [[x y] (transform-coords coords tx)]
    (-> grid
        (nth y)
        (subs x (inc x)))))

(defn show [grid tx]
  (doseq [y (range tile-size)]
    (doseq [x (range tile-size)]
      (print (tile-at grid [x y] tx)))
    (println)))

(defn border-e [grid tx]
  (-> (for [y (range tile-size)]
        (tile-at grid [(dec tile-size) y] tx))
      (str/join)))

(defn border-n [grid tx]
  (-> (for [x (range tile-size)]
        (tile-at grid [x 0] tx))
      (str/join)))

(defn border-s [grid tx]
  (-> (for [x (range tile-size)]
        (tile-at grid [x (dec tile-size)] tx))
      (str/join)))

(defn border-w [grid tx]
  (-> (for [y (range tile-size)]
        (tile-at grid [0 y] tx))
      (str/join)))

(defn all-borders-tx [grid tx]
  [(border-e grid tx)
   (border-n grid tx)
   (border-s grid tx)
   (border-w grid tx)])

(defn all-borders [grid]
  (->> (mapcat all-borders-tx (repeat grid) all-txs)
       (into #{})))

(defn matches [tile-borders [tile-id borders]]
  [tile-id (->> (for [[other-tile-id other-borders] tile-borders
                      :when (not= other-tile-id tile-id)
                      :let [common-borders (set/intersection borders other-borders)]
                      :when (pos? (count common-borders))]
                  other-tile-id)
                (into #{}))])

(defn run []
  (let [tiles (parse-tiles)
        tile-borders (map-vals all-borders tiles)]
    (->> (map #(matches tile-borders %) tile-borders)
         (filter #(= (-> % last count) 2))
         (map first)
         (reduce *))))
