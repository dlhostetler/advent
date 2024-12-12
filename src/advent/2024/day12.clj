(ns advent.2024.day12
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def init-plots
  (->> "resources/2024/day12.input"
       grid/slurp
       (map-vals str)))

(defn region-at [plots [p plant]]
  (->> (grid/flood plots {:fill-with "*"
                          :should-fill? (partial = plant)
                          :start-point p})
       :grid
       (filter (comp (partial = "*") val))
       (map-vals (constantly plant))))

(defn split-regions [plots]
  (loop [plots plots
         regions []]
    (if (empty? plots)
      regions
      (let [region (region-at plots (-> plots first))]
        (recur (apply dissoc plots (keys region))
               (conj regions region))))))

(defn area [region]
  (count region))

(defn count-fences [region [p]]
  (->> [(grid/north p) (grid/east p) (grid/south p) (grid/west p)]
       (map #(contains? region %))
       (filter true?)
       count
       (- 4)))

(defn perimeter [region]
  (->> region
       sort
       (map (partial count-fences region))
       (reduce +)))

(defn price [region]
  (* (area region) (perimeter region)))

(defn run []
  (->> init-plots
       split-regions
       (map price)
       (reduce +)))
