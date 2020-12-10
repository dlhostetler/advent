(ns advent.2020.day10
  (:require [clojure.java.io :as io]))

(defn read-adapters []
  (->> "resources/2020/day10.input"
       io/reader
       line-seq
       (map #(Integer/parseInt %))
       (into #{})))

(def possibilities
  (memoize (fn [adapters joltage]
             (cond
               (= joltage 0) 1
               (< joltage 0) 0
               (not (contains? adapters joltage)) 0
               :else (+ (possibilities adapters (dec joltage))
                        (possibilities adapters (- joltage 2))
                        (possibilities adapters (- joltage 3)))))))

(defn run []
  (let [adapters (read-adapters)
        max-adapter (apply max adapters)]
    (possibilities adapters max-adapter)))
