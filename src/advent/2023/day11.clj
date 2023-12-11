(ns advent.2023.day11
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all]))

(def empty-space \.)
(def galaxy-space \#)

(def initial-universe
  (-> "resources/2023/day11.input"
      io/reader
      grid/slurp))

(defn empty-at? [universe point]
  (= (get universe point) empty-space))

(defn galaxy? [[_ space]]
  (= space galaxy-space))

(defn empty-col? [universe x]
  (->> (for [y (range (inc (grid/max-y universe)))]
         [x y])
       (every? (partial empty-at? universe))))

(defn expand-col [universe target-x]
  (let [target-x+1 (inc target-x)
        new-col (into {}
                      (for [y (range (inc (grid/max-y universe)))]
                        [[target-x+1 y] empty-space]))]
    (->> universe
         (map-keys (fn [[x y]]
                     (if (> x target-x) [(inc x) y] [x y])))
         (merge new-col))))

(defn expand-cols
  ([universe]
   (expand-cols universe (grid/max-x universe)))
  ([universe x]
   (if (>= x 0)
     (recur (if (empty-col? universe x)
              (expand-col universe x)
              universe)
            (dec x))
     universe)))

(defn empty-row? [universe y]
  (->> (for [x (range (inc (grid/max-x universe)))]
         [x y])
       (every? (partial empty-at? universe))))

(defn expand-row [universe target-y]
  (let [target-y+1 (inc target-y)
        new-row (into {}
                      (for [x (range (inc (grid/max-x universe)))]
                        [[x target-y+1] empty-space]))]
    (->> universe
         (map-keys (fn [[x y]]
                     (if (> y target-y) [x (inc y)] [x y])))
         (merge new-row))))

(defn expand-rows
  ([universe]
   (expand-rows universe (grid/max-y universe)))
  ([universe y]
   (if (>= y 0)
     (recur (if (empty-row? universe y)
              (expand-row universe y)
              universe)
            (dec y))
     universe)))

(defn expand-universe [universe]
  (-> universe
      expand-cols
      expand-rows))

(defn ->galaxies [universe]
  (->> universe
       (filter galaxy?)
       (map first)))

(defn coords-diff [point0 point1]
  (mapv - point0 point1))

(defn pair-manhattan [[point0 point1]]
  (->> (coords-diff point0 point1)
       (map #(Math/abs ^int %))
       (reduce +)))

(defn run []
  (let [universe (expand-universe initial-universe)
        galactic-pairs (-> universe ->galaxies (combo/combinations 2))]
    (println "there are" (count galactic-pairs) "pairs")
    (->> galactic-pairs
         (pmap pair-manhattan)
         (reduce +))))
