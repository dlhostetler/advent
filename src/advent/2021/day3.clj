(ns advent.2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn input []
  (-> "resources/2021/day3.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn bit-counts [nums i]
  (->> nums
       (map #(nth % i))
       (map str)
       (map #(Integer/parseInt %))
       (group-by identity)
       (map-vals count)))

(defn next-prefix [nums prefix comparison]
  (let [i (count prefix)
        {zero-count 0 one-count 1} (bit-counts nums i)]
    (if (comparison zero-count one-count)
      (str prefix "1")
      (str prefix "0"))))

(defn ->rating [nums comparison]
  (loop [nums nums
         prefix ""]
    (if (> (count nums) 1)
      (let [next-prefix (next-prefix nums prefix comparison)]
        (recur (filter #(str/starts-with? % next-prefix) nums)
               next-prefix))
      (first nums))))

(defn bits-to-int [bits]
  (Integer/parseInt (apply str bits) 2))

(defn run []
  (let [nums (input)
        o2 (bits-to-int (->rating nums <=))
        co2 (bits-to-int (->rating nums >))]
    (* o2 co2)))
