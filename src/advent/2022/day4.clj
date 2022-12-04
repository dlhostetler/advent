(ns advent.2022.day4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (-> "resources/2022/day4.input"
      io/reader
      line-seq))

(defn parse-pairs [s]
  (str/split s #","))

(defn parse-section [section]
  (let [[start end] (->> (str/split section #"-")
                         (mapv #(Integer/parseInt %)))]
    (set (range start (inc end)))))

(defn parse-sections [pair]
  (map parse-section pair))

(defn fully-contained? [sections]
  (let [x (apply set/intersection sections)]
    (->> sections
         (map #(= x %))
         (filter identity)
         first)))

(defn run []
  (->> input
       (map parse-pairs)
       (map parse-sections)
       (map fully-contained?)
       (filter identity)
       count))
