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

(defn split-grid-at [mirror x]
  (let [splits (->> mirror
                    (group-by (fn [[point]]
                                (< (grid/x point) x)))
                    (map-vals #(into {} %)))]
    [(get splits true) (get splits false)]))

(defn col [mirror x]
  (->> mirror
       (filter (comp (partial = x) grid/x first))
       (sort-by (comp grid/y first))
       vals))

(defn cols [mirror xs]
  (into [] (for [x xs] (col mirror x))))

(defn mirrored? [[left right]]
  (let [left-lines (cols left
                         (range (grid/max-x left)
                                (dec (grid/min-x left))
                                -1))
        right-lines (cols right
                          (range (grid/min-x right)
                                 (inc (grid/max-x right))))]
    (->> (map count-differences left-lines right-lines)
         (reduce +)
         (= 1))))

(defn grid-mirrored-at [mirror]
  (->> (for [x (range (inc (grid/min-x mirror)) (inc (grid/max-x mirror)))
             :when (-> mirror
                       (split-grid-at x)
                       mirrored?)]
         x)
       first))

(defn run []
  (let [verticals (->> mirrors
                       (map grid-mirrored-at)
                       (remove nil?))
        horizontals (->> mirrors
                         (map grid/rotate-90-neg)
                         (map grid-mirrored-at)
                         (remove nil?))]
    (->> verticals
         (concat (map (partial * 100) horizontals))
         (reduce +))))
