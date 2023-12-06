(ns advent.2023.day6
  (:require [plumbing.core :refer :all]))

(def test-input
  [{:distance 940200
    :time 71530}])

(def input
  [{:distance 298118510661181
    :time 49787980}])

(defn ->distances [{time :time}]
  (for [t (range (inc time))]
    (* t (- time t))))

(defn ->winning-margin [outcomes {distance :distance}]
  (->> outcomes
       (filter #(> % distance))
       count))

(defn run []
  (let [all-outcomes (mapv ->distances input)]
    (->> input
         (map ->winning-margin all-outcomes)
         (reduce *))))
