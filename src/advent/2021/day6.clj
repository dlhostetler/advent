(ns advent.2021.day6
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn fishes []
  (->> (-> "resources/2021/day6.input"
           io/reader
           slurp
           (str/split #","))
       (map #(Integer/parseInt %))
       (group-by identity)
       (map-vals count)))

(defn next-fishes [fishes [countdown num]]
  (if (zero? countdown)
    (-> fishes
        (update 8 (fnil + 0) num)
        (update 6 (fnil + 0) num))
    (update fishes
            (dec countdown)
            (fnil + 0)
            num)))

(defn next-fisheses [fishes]
  (reduce next-fishes {} fishes))

(defn run []
  (->> (fishes)
       (seq/successive next-fisheses)
       (drop 256)
       first
       vals
       (reduce +)))
