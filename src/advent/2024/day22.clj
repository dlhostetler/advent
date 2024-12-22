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

(defn run []
  (->> input
       (map secrets)
       (map #(drop 2000 %))
       (map first)
       (reduce +)))
