(ns advent.2021.day11
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all]
            [advent.seq :as seq]))

(defn by-coordinates [grid]
  (->> (for [y (range (count grid))
             x (range (-> grid first count))]
         [[x y] (-> grid
                    (nth y)
                    (nth x))])
       (into {})
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(def orig-octopi
  (->> "resources/2021/day11.input"
       io/reader
       line-seq
       (mapv vec)
       by-coordinates))

(def max-x
  (->> orig-octopi keys (map first) (apply max)))

(def max-y
  (->> orig-octopi keys (map last) (apply max)))

(defn valid-x? [x]
  (and (>= x 0) (<= x max-x)))

(defn valid-y? [y]
  (and (>= y 0) (<= y max-y)))

(defn neighbor-point [[from-x from-y] [x-offset y-offset]]
  (loop [x (+ from-x x-offset)
         y (+ from-y y-offset)]
    (when (and (valid-x? x) (valid-y? y))
      [x y])))

(defn neighbor-points [point]
  (->> (-> (combo/cartesian-product [-1 0 1] [-1 0 1])
           set
           (disj [0 0]))
       (map #(neighbor-point point %))
       (filter some?)))

(alter-var-root #'neighbor-points memoize)

(defn normalize-octopus [[point octopus]]
  [point (if (> octopus 9) 0 octopus)])

(defn next-state [starting-octopi]
  (loop [octopi starting-octopi
         to-poke (keys starting-octopi)
         flashed #{}]
    (if-not (empty? to-poke)
      (let [point (first to-poke)
            next-octopus (inc (get octopi point))
            flash? (and (> next-octopus 9) (not (flashed point)))
            next-to-poke (-> to-poke
                             rest
                             (into (if flash?
                                     (neighbor-points point)
                                     [])))]
        (recur (assoc octopi point next-octopus)
               next-to-poke
               (if flash? (conj flashed point) flashed)))
      (->> octopi
           (map normalize-octopus)
           (into {})))))

(defn count-flashes [octopi]
  (->> octopi vals (filter #(= 0 %)) count))

(defn run []
  (->> orig-octopi
       (seq/successive next-state)
       (take 101)
       (map count-flashes)
       (reduce +)))
