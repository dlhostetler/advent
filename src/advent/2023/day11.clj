(ns advent.2023.day11
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all]))
(def expansion (dec 1000000))

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

(defn empty-cols
  ([universe]
   (empty-cols universe [] (grid/max-x universe)))
  ([universe empties x]
   (if (>= x 0)
     (recur universe
            (if (empty-col? universe x)
              (conj empties x)
              empties)
            (dec x))
     empties)))

(defn expand-col [galaxies target-x]
  (into #{}
        (for [[x y :as galaxy] galaxies]
          (if (> x target-x)
            [(+ x expansion) y]
            galaxy))))

(defn expand-cols [galaxies]
  (reduce expand-col galaxies (empty-cols initial-universe)))

(defn empty-row? [universe y]
  (->> (for [x (range (inc (grid/max-x universe)))]
         [x y])
       (every? (partial empty-at? universe))))

(defn empty-rows
  ([universe]
   (empty-rows universe [] (grid/max-y universe)))
  ([universe empties y]
   (if (>= y 0)
     (recur universe
            (if (empty-row? universe y)
              (conj empties y)
              empties)
            (dec y))
     empties)))

(defn expand-row [galaxies target-y]
  (into #{}
        (for [[x y :as galaxy] galaxies]
          (if (> y target-y)
            [x (+ y expansion)]
            galaxy))))

(defn expand-rows [galaxies]
  (reduce expand-row galaxies (empty-rows initial-universe)))

(defn expand-universe [galaxies]
  (-> galaxies
      expand-cols
      expand-rows))

(defn ->galaxies [universe]
  (->> universe
       (filter galaxy?)
       (map first)
       (into #{})))

(defn coords-diff [point0 point1]
  (mapv - point0 point1))

(defn pair-manhattan [[point0 point1]]
  (->> (coords-diff point0 point1)
       (map #(Math/abs ^int %))
       (reduce +)))

(defn run []
  (let [galaxies (-> initial-universe
                     (->galaxies)
                     expand-universe)
        galactic-pairs (combo/combinations galaxies 2)]
    (println "there are" (count galactic-pairs) "pairs")
    (->> galactic-pairs
         (pmap pair-manhattan)
         (reduce +))))
