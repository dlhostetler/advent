(ns advent.2023.day6
  (:require [plumbing.core :refer :all]))

(def test-input
  [{:distance 9
    :time 7}
   {:distance 40
    :time 15}
   {:distance 200
    :time 30}])

(def input
  [{:distance 298
    :time 49}
   {:distance 1185
    :time 78}
   {:distance 1066
    :time 79}
   {:distance 1181
    :time 80}])

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
