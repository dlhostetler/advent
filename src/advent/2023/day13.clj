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

(defn split-grid-at [mirror coordinate n]
  (let [splits (->> mirror
                    (group-by (fn [[point]]
                                (< (grid/val-coord coordinate point) n)))
                    (map-vals #(into {} %)))]
    [(get splits true) (get splits false)]))

(defn line [mirror coordinate n]
  (->> mirror
       (filter (comp (partial = n) #(grid/val-coord coordinate %) first))
       (sort-by (comp #(grid/opp-coord coordinate %) first))
       vals))

(defn lines [mirror coordinate ns]
  (into [] (for [n ns]
             (line mirror coordinate n))))

(defn mirrored? [[half1 half2] coordinate]
  (let [half0-lines (lines half1
                           coordinate
                           (range (grid/max-coord coordinate half1)
                                  (dec (grid/min-coord coordinate half1))
                                  -1))
        half1-lines (lines half2
                           coordinate
                           (range (grid/min-coord coordinate half2)
                                  (inc (grid/max-coord coordinate half2))))]
    (->> (map count-differences half0-lines half1-lines)
         (reduce +)
         (= 1))))

(defn grid-mirrored-at [mirror coordinate]
  (->> (for [n (range (inc (grid/min-coord coordinate mirror))
                      (inc (grid/max-coord coordinate mirror)))
             :when (-> mirror
                       (split-grid-at coordinate n)
                       (mirrored? coordinate))]
         n)
       first))

(defn run []
  (let [verticals (->> mirrors
                       (map #(grid-mirrored-at % (grid/->XCoordinate)))
                       (remove nil?))
        horizontals (->> mirrors
                         (map #(grid-mirrored-at % (grid/->YCoordinate)))
                         (remove nil?))]
    (->> verticals
         (concat (map (partial * 100) horizontals))
         (reduce +))))
