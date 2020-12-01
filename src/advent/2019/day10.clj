(ns advent.2019.day10
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))


(defn parse-asteroid [x y c]
  (when (= \# c)
    [x y]))

(defn parse-row [y row]
  (map-indexed #(parse-asteroid %1 y %2) row))

(defn parse-field []
  (->> "resources/2019/day10.input"
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
  {:angle (angle asteroid-point target-point)
   :distance (distance asteroid-point target-point)
   :point target-point})

(defn analyze-field [field asteroid-point]
  (mapv analyze-asteroid (repeat asteroid-point) field))

(defn pass [i angles points-by-angle]
  (->> (for [angle angles
             :let [points (get points-by-angle angle)]
             :when (< i (count points))]
         (nth points i))))

;; Test - 11 13
;; Real - 20 19

(defn run []
  (let [field (parse-field)
        analyzed-field (->> (analyze-field field [20 19])
                            (remove (comp zero? :distance))
                            (group-by :angle)
                            (map-vals #(sort-by :distance %))
                            (map-vals #(map :point %)))
        angles (into #{} (keys analyzed-field))
        angles (concat (->> angles
                            (filter zero?)
                            sort)
                       (->> angles
                            (filter neg?)
                            sort
                            reverse)
                       (->> angles
                            (filter pos?)
                            sort
                            reverse))
        max-points (->> analyzed-field
                        vals
                        (map count)
                        (apply max))]
    (into [] (mapcat pass (range max-points) (repeat angles) (repeat analyzed-field)))))