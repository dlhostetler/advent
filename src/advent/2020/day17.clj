(ns advent.2020.day17
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [plumbing.core :refer :all]))

(def active-tile \#)
(def inactive-tile \.)
(def dimensions 3)

(defn pad-coords [coords]
  (->> (repeat (- dimensions (count coords)) 0)
       (concat coords)
       (into [])))

(defn by-coordinates [grid]
  (->> (for [y (range (count grid))
             x (range (-> grid first count))]
         [[x y] (-> grid
                    (nth y)
                    (nth x))])
       (map-keys pad-coords)
       (into {})))

(defn parse-layout []
  (->> "resources/2020/day17.input"
       io/reader
       line-seq
       (mapv vec)
       by-coordinates))

(defn tile-at [layout coords]
  (get layout coords inactive-tile))

(def neighbors
  (memoize
    (fn [coords]
      (let [offsets (->> [-1 0 1]
                         (repeat dimensions)
                         (apply combo/cartesian-product))]
        (->> (map (fn [offset]
                    (mapv + coords offset))
                  offsets)
             set)))))

(defn all-coords [layout]
  (->> layout
       keys
       (mapcat neighbors)
       set))

(defn count-active-neighbors [layout coords]
  (->> (disj (neighbors coords) coords)
       (map #(tile-at layout %))
       (filter #(= active-tile %))
       count))

(defn next-tile [layout coords]
  (let [tile (tile-at layout coords)
        num-active-neighbors (count-active-neighbors layout coords)]
    (cond
      (and (= tile active-tile)
           (or (= num-active-neighbors 2)
               (= num-active-neighbors 3)))
      active-tile
      (and (= tile inactive-tile)
           (= num-active-neighbors 3))
      active-tile
      :else
      inactive-tile)))

(defn next-layout [layout]
  (->> (for [coords (all-coords layout)]
         [coords (next-tile layout coords)])
       (into {})))

(defn run []
  (->> (parse-layout)
       (seq/successive next-layout)
       (take 7)
       last
       vals
       (filter #(= active-tile %))
       count))
