(ns advent.2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn ->bits [s]
  (->> s
       vec
       (map str)
       (map #(Integer/parseInt %))))

(defn input []
  (->> (-> "resources/2021/day3.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map ->bits)))

(defn transpose [m]
  (apply mapv vector m))

(defn ->bit-by-count [bits]
  (->> bits
       (group-by identity)
       (map-vals count)))

(defn ->bit [comparison {zero-count 0 one-count 1}]
  (if (comparison zero-count one-count) 1 0))

(defn bits-to-int [bits]
  (Integer/parseInt (apply str bits) 2))

(defn run []
  (let [bits-by-count (->> (input) (transpose) (map ->bit-by-count))]
    (* (bits-to-int (map (partial ->bit <=) bits-by-count)) ;; epsilon
       (bits-to-int (map (partial ->bit >) bits-by-count))))) ;; gamma
