(ns advent.2021.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]
            [clojure.set :as set]))

(defn input []
  (->> (-> "resources/2021/day7.input"
           io/reader
           slurp
           (str/split #","))
       (map #(Integer/parseInt %))))

(defn crab->fuel [crab position]
  (Math/abs ^long (- crab position)))

(defn crabs->fuels [crabs position]
  (map #(crab->fuel % position) crabs))

(defn total-fuel [crabs position]
  (reduce + (crabs->fuels crabs position)))

(defn run []
  (let [crabs (input)
        fuel->position (->> (for [position (range (inc (apply max crabs)))]
                              [position (total-fuel crabs position)])
                            (into {})
                            set/map-invert)]
    (->> fuel->position keys (apply min))))
