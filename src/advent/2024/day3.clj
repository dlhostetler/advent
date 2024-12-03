(ns advent.2024.day3
  (:require [clojure.java.io :as io]))

(def input
  (-> "resources/2024/day3.input"
      io/reader
      slurp))

(def operation-matcher
  (re-matcher #"mul[(](\d+),(\d+)[)]" input))

(defn next-operation []
  (when-let [[_ x y] (re-find operation-matcher)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(def operations
  (->> (repeatedly next-operation)
       (take-while some?)))

(defn run []
  (->> operations
       (map #(reduce * %))
       (reduce +)))
