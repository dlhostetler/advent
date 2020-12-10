(ns advent.2020.day10
  (:require [clojure.java.io :as io]))

(defn read-adapters []
  (->> "resources/2020/day10.input"
       io/reader
       line-seq
       (map #(Integer/parseInt %))
       (into #{})))

(defn differences [joltages]
  (map #(- %2 %1)
       (butlast joltages)
       (rest joltages)))

(defn answer [difference-counts]
  (* (get difference-counts 1)
     (get difference-counts 3)))

(defn run []
  (let [adapters (read-adapters)
        device-joltage (->> adapters
                            (apply max)
                            (+ 3))
        joltages (concat [0] adapters [device-joltage])]
    (->> joltages
         (sort)
         (differences)
         (frequencies)
         (answer))))
