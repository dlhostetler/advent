(ns advent.2023.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2023/day1.input"
      io/reader
      slurp
      (str/split #"\n")))

(def w-to-n
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def digits (concat (keys w-to-n) (vals w-to-n)))

(defn first-digit-at [s digit]
  (when-let [i (str/index-of s digit)]
    [i digit]))

(defn last-digit-at [s digit]
  (when-let [i (str/last-index-of s digit)]
    [i digit]))

(defn digit-by [s index-fn position-fn]
  (->> (map #(index-fn s %) digits)
       (remove nil?)
       (sort-by first)
       position-fn
       last))

(defn first-digit [s]
  (let [digit (digit-by s first-digit-at first)]
    (or (w-to-n digit) digit)))

(defn last-digit [s]
  (let [digit (digit-by s last-digit-at last)]
    (or (w-to-n digit) digit)))

(defn to-number [s]
  (Integer/parseInt (str (first-digit s) (last-digit s))))

(defn run []
  (->> input
       (map to-number)
       (reduce +)))
