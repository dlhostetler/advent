(ns advent.2021.day11
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [plumbing.core :refer :all]))

(def orig-octopi
  (->> "resources/2021/day11.input"
       grid/slurp
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(defn normalize-octopus [octopus]
  (if (> octopus 9) 0 octopus))

(defn tx-octopus [point {:keys [grid txed?]}]
  (let [next-octopus (inc (get grid point))
        flash? (and (> next-octopus 9) (not (txed? point)))]
    {:also-tx (when flash? (grid/eight-neighbors grid point))
     :value next-octopus
     :revisit? (not flash?)}))

(defn next-state [octopi]
  (->> octopi
       (grid/tx-points tx-octopus)
       (map-vals normalize-octopus)))

(defn count-flashes [octopi]
  (->> octopi vals (filter #(= 0 %)) count))

(defn run []
  (->> orig-octopi
       (seq/successive next-state)
       (take 101)
       (map count-flashes)
       (reduce +)))
