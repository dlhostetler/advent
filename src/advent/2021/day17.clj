(ns advent.2021.day17
  (:require [advent.seq :as seq]
            [plumbing.core :refer :all]))

;; target area: x=230..283, y=-107..-57
(def target-x [230 283])
(def max-x (apply max target-x))
(def target-y [-107 -57])
(def min-y (apply min target-y))

(defn drag-velocity [x]
  (cond
    (pos? x)
    (- x 1)
    (neg? x)
    (+ x 1)
    :else
    x))

(defn next-position [{:keys [velocity] :as state}]
  (-> state
      (update-in [:position 0] + (first velocity))
      (update-in [:position 1] + (last velocity))
      (update-in [:velocity 0] drag-velocity)
      (update-in [:velocity 1] dec)))

(defn in-target? [path]
  (let [position (last path)]
    (and (<= (first target-x) (first position) (last target-x))
         (<= (first target-y) (last position) (last target-y)))))

(defn past-target? [{:keys [position]}]
  (or (> (first position) max-x)
      (< (last position) min-y)))

(defn path [velocity]
  (->> {:position [0 0]
        :velocity velocity}
       (seq/successive next-position)
       (take-while (comp not past-target?))
       (mapv :position)))

(defn velocities [from-x to-x from-y to-y]
  (for [x (range from-x to-x)
        y (range from-y to-y)]
    [x y]))

(defn highest-y [path]
  (->> path
       (map last)
       (reduce max)))

(defn run []
  (->> (velocities 0 1000 0 1000)
       (map path)
       (filter in-target?)
       (map highest-y)
       (reduce max)))
