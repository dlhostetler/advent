(ns advent.2022.day6
  (:require [plumbing.core :refer :all]))

(def marker-length 14)

(def input
  (slurp "resources/2022/day6.input"))

(defn start-of-packet? [characters]
  (= (-> characters set count) marker-length))

(defn run []
  (->> input
       seq
       (partition marker-length 1)
       (take-while (comp not start-of-packet?))
       count
       (+ marker-length)))
