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

(defn overlap? [sections]
  (->> sections
       (apply set/intersection)
       empty?
       not))

(defn run []
  (->> input
       (map parse-pairs)
       (map parse-sections)
       (map overlap?)
       (filter identity)
       count))
