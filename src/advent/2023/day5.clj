(ns advent.2023.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def path [:seed
           :soil
           :fertilizer
           :water
           :light
           :temperature
           :humidity
           :location])

(def path-pairs (map vec (partition 2 1 path)))

(def input
  (-> "resources/2023/day5.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn s->longs [s]
  (->> s (re-seq #"\d+") (mapv #(Long/parseLong %))))

(def seeds (-> input first s->longs))

(def seed-pairs (map vec (partition 2 seeds)))

(defn parse-map [s]
  (let [lines (str/split s #"\n")
        [_ from to] (->> lines
                         first
                         (re-matches #"^([a-z]+)-to-([a-z]+).*"))
        ranges (into []
                     (for [r (->> lines rest (map s->longs))]
                       r))]
    [[(keyword from) (keyword to)] ranges]))

(def almanac (->> input
                  rest
                  (map parse-map)
                  (into {})))

(defn from-range [[dest-start source-start len] n]
  (when (<= source-start n (+ source-start len))
    (+ (- dest-start source-start) n)))

(defn mapping [ranges n]
  (or (->> (map from-range ranges (repeat n)) (remove nil?) first)
      n))

(defn lookup [n from-to]
  (mapping (get almanac from-to) n))

(defn seed->location [n]
  (reduce lookup n path-pairs))

(defn min-location [[start len]]
  (loop [i 0
         location nil]
    (when (zero? (mod i 1000000))
      (println start "." i))
    (let [l (seed->location (+ start i))]
      (if (< i len)
        (recur (inc i) (if location (min location l) l))
        (do
          (println "min location for" start "is" location)
          location)))))

(defn run []
  (->> seed-pairs
       (pmap min-location)
       (reduce min)))
