(ns advent.2022.day15
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [plumbing.core :refer :all]))

(def target-y 2000000)

(defn target-y? [[_ y]]
  (= y target-y))

(defn parse-line [s]
  (let [[_ xb yb xs ys] (re-matches #"Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)" s)]
    [[(Integer/parseInt xb) (Integer/parseInt yb)]
     [(Integer/parseInt xs) (Integer/parseInt ys)]]))

(defn coords-diff [point0 point1]
  (mapv - point0 point1))

(defn manhattan [point0 point1]
  (->> (coords-diff point0 point1)
       (map #(Math/abs ^int %))
       (reduce +)))

(def sensor->beacon
  (->> "resources/2022/day15.input"
       io/reader
       line-seq
       (map parse-line)
       (into {})))

(def beacon-xs (->> sensor->beacon
                    (map last)
                    (filter target-y?)
                    (map last)
                    set))

(defn to-x-range [[[xs ys :as sensor] beacon]]
  (let [d (manhattan sensor beacon)
        r (- d (Math/abs ^long (- target-y ys)))]
    (when (>= r 0)
      [(- xs r) (+ xs r)])))

(defn explode-range [[from to]]
  (set (range from (inc to))))

(defn run []
  (->> sensor->beacon
       (map to-x-range)
       (filter some?)
       (map explode-range)
       (apply set/union)
       (#(set/difference % beacon-xs))
       count))
