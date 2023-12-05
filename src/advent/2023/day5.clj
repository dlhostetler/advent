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

(defn run []
  (->> seeds
       (map seed->location)
       (reduce min)))
