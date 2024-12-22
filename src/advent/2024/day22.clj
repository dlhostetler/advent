(ns advent.2024.day22
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map #(Integer/parseInt %)
       (-> "resources/2024/day22.input"
           io/reader
           slurp
           (str/split #"\n"))))

(defn mix [secret fn]
  (bit-xor secret (fn secret)))

(defn prune [secret]
  (mod secret 16777216))

(defn next-secret [secret]
  (-> secret
      ;; 1
      (mix #(* % 64))
      prune
      ;; 2
      (mix #(int (/ % 32)))
      prune
      ;; 3
      (mix #(* % 2048))
      prune))

(defn secrets [start]
  (seq/successive next-secret start))

(defn last-digit [n]
  (mod n 10))

(defn changes [s]
  (->> s
       (partition 2 1)
       (mapv (fn [[x y]] (- y x)))))

(defn price [prices group]
  (let [k (changes group)
        p (last group)]
    (if (not (prices k))
      ;; the monkey pays out the first time it sees the sequence
      (assoc prices k p)
      ;; there's already a higher price
      prices)))

(defn prices [start]
  (let [groups (->> start
                    secrets
                    (take 2001)
                    (map last-digit)
                    (partition 5 1))]
    (reduce price {} groups)))

(defn all-changes [all-prices]
  (->> all-prices
       (map keys)
       (apply concat)
       (into #{})))

(defn total-bananas [all-prices changes]
  (->> all-prices
       (map #(get % changes))
       (remove nil?)
       (reduce +)))

(defn run []
  (let [all-prices (map prices input)]
    (->> (all-changes all-prices)
         (map (partial total-bananas all-prices))
         (reduce max))))
