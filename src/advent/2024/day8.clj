(ns advent.2024.day8
  (:require [advent.grid :as grid]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all]))

(defn into-map-of-sets [m [p freq]]
  (update m freq (fnil conj #{}) p))

(def space
  (->> "resources/2024/day8.input"
       grid/slurp
       (map-vals str)))

(def freqs
  (->> space
       (remove (comp (partial = ".") val))
       (reduce into-map-of-sets {})))

(defn back [p diff-x diff-y]
  (loop [[x y :as p] p
         ps []]
    (if (grid/valid-point-or-nil space p)
      (let [next-p [(+ x (* -1 diff-x)) (+ y (* -1 diff-y))]]
        (recur next-p (conj ps next-p)))
      ps)))

(defn forward [p diff-x diff-y]
  (loop [[x y :as p] p
         ps []]
    (if (grid/valid-point-or-nil space p)
      (let [next-p [(+ x diff-x) (+ y diff-y)]]
        (recur next-p (conj ps next-p)))
      ps)))

(defn pair-antinodes [points]
  (let [[[x0 y0] [x1 y1]] (sort points)
        diff-x (- x0 x1)
        diff-y (- y0 y1)]
    (concat (back (last points) diff-x diff-y)
            [[x0 y0]]
            [[x1 y1]]
            (forward (first points) diff-x diff-y))))

(defn antinodes [points]
  (->> (combo/combinations points 2)
       (map pair-antinodes)
       (apply concat)
       (filter (partial grid/valid-point-or-nil space))))

(defn run []
  (let [ans (->> freqs
                 vals
                 (map antinodes)
                 (apply concat))]
    #_(grid/print (merge space
                       (zipmap ans (repeat "#"))))
    (->> ans (into #{}) count)))
