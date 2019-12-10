(ns advent.2019.day10
  (:require [clojure.java.io :as io]))


(defn parse-asteroid [x y c]
  (when (= \# c)
    [x y]))

(defn parse-row [y row]
  (map-indexed #(parse-asteroid %1 y %2) row))

(defn parse-field []
  (->> "resources/day10.input"
       io/reader
       line-seq
       (map seq)
       (map-indexed parse-row)
       (mapcat identity)
       (remove nil?)
       (into [])))

(defn angle [asteroid-point target-point]
  (let [delta-x (- (double (first asteroid-point))
                   (double (first target-point)))
        delta-y (- (double (second asteroid-point))
                   (double (second target-point)))]
    (Math/atan2 delta-x delta-y)))

(defn distance [asteroid-point target-point]
  (let [delta-x (- (double (first asteroid-point))
                   (double (first target-point)))
        delta-y (- (double (second asteroid-point))
                   (double (second target-point)))]
    (+ (Math/abs delta-x) (Math/abs delta-y))))

(defn analyze-asteroid [asteroid-point target-point]
  [target-point {:angle (angle asteroid-point target-point)
                 :distance (distance asteroid-point target-point)}])

(defn analyze-field [field asteroid-point]
  (->> field
       (map analyze-asteroid (repeat asteroid-point))
       (into {})))

(defn num-angles [analyzed-field]
  (->> analyzed-field
       vals
       (remove (comp zero? :distance))
       (map :angle)
       (into #{})
       count))

(defn run []
  (let [field (parse-field)]
    (->> field
         (map analyze-field (repeat field))
         (map num-angles)
         (apply max))))