(ns advent.2023.day13
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def mirrors
  (->> (-> "resources/2023/day13.input"
           io/reader
           slurp
           (str/split #"\n\n"))
       (map #(str/split % #"\n"))
       (map grid/lines->grid)))

(defn count-differences [v0 v1]
  (->> (map = v0 v1)
       (remove true?)
       count))

(defn split-at-x [mirror x]
  (let [splits (->> mirror
                    (group-by (fn [[point]]
                                (< (first point) x)))
                    (map-vals #(into {} %)))]
    [(get splits true) (get splits false)]))

(defn col [mirror x]
  (->> mirror
       (filter (comp (partial = x) first first))
       (sort-by (comp last first))
       vals))

(defn cols [mirror xs]
  (into []
        (for [x xs]
          (col mirror x))))

(defn mirrored-x? [[left right]]
  (let [left-cols (cols left
                        (range (grid/max-x left) (dec (grid/min-x left)) -1))
        right-cols (cols right
                         (range (grid/min-x right) (inc (grid/max-x right))))]
    (->> (map count-differences left-cols right-cols)
         (reduce +)
         (= 1))))

(defn mirrored-at-x [mirror]
  (->> (for [x (range (inc (grid/min-x mirror))
                      (inc (grid/max-x mirror)))
             :when (-> mirror (split-at-x x) mirrored-x?)]
         x)
       first))

(defn split-at-y [mirror y]
  (let [splits (->> mirror
                    (group-by (fn [[point]]
                                (< (last point) y)))
                    (map-vals #(into {} %)))]
    [(get splits true) (get splits false)]))

(defn row [mirror y]
  (->> mirror
       (filter (comp (partial = y) last first))
       (sort-by (comp first first))
       vals))

(defn rows [mirror ys]
  (into []
        (for [y ys]
          (row mirror y))))

(defn mirrored-y? [[top bottom]]
  (let [top-rows (rows top
                       (range (grid/max-y top) (dec (grid/min-y top)) -1))
        bottom-rows (rows bottom
                          (range (grid/min-y bottom) (inc (grid/max-y bottom))))]
    (->> (map count-differences top-rows bottom-rows)
         (reduce +)
         (= 1))))

(defn mirrored-at-y [mirror]
  (->> (for [y (range (inc (grid/min-y mirror))
                      (inc (grid/max-y mirror)))
             :when (-> mirror (split-at-y y) mirrored-y?)]
         y)
       first))

(defn run []
  (let [verticals (->> mirrors (map mirrored-at-x) (remove nil?))
          horizontals (->> mirrors (map mirrored-at-y) (remove nil?))]
      (->> verticals
           (concat (map (partial * 100) horizontals))
           (reduce +))))
