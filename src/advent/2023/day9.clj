(ns advent.2023.day9
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-history [line]
  (->> (str/split line #" ")
       (map #(Integer/parseInt %))))

(def histories
  (->> (-> "resources/2023/day9.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-history)))

(defn next-sequence [history]
  (->> history
       (partition 2 1)
       (map (comp #(apply - %) reverse))))

(defn not-all-zeros? [nums]
  (not (every? zero? nums)))

(defn ->pattern [history]
  (->> history
       (seq/successive next-sequence)
       (seq/take-while+1 not-all-zeros?)
       (map last)))

(defn next-value [pattern]
  (reduce + 0 pattern))

(defn run []
  (->> histories
       (map ->pattern)
       (map next-value)
       (reduce +)))
