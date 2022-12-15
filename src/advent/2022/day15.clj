(ns advent.2022.day15
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def target-range 4000000)

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

(defn possible-points [[[xs ys :as sensor] beacon]]
  (let [d (manhattan sensor beacon)
        d+1 (inc d)]
    (for [x-dir (range -1 (inc 1))
          y-dir (range -1 (inc 1))
          xd (range 0 (inc d+1))
          :let [yd (- d+1 xd)
                x (* (+ xs xd) x-dir)
                y (* (+ ys yd) y-dir)]
          :when (not (or (< x 0)
                         (> x target-range)
                         (< y 0)
                         (> y target-range)))]
      [x y])))

(defn closer-than-sensed? [possible-beacon [sensor beacon]]
  (let [real-beacon-distance (manhattan sensor beacon)
        possible-beacon-distance (manhattan sensor possible-beacon)]
    (<= possible-beacon-distance real-beacon-distance)))

(defn new-beacon? [possible-beacon]
  (->> sensor->beacon
       (map (partial closer-than-sensed? possible-beacon))
       (every? false?)))

(defn tuning-frequency [[x y]]
  (+ (* x 4000000) y))

(defn run []
  (->> sensor->beacon
       (mapcat possible-points)
       (filter new-beacon?)
       first
       tuning-frequency))
