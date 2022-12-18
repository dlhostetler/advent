(ns advent.2022.day18
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-cube [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(def cubes
  (->> "resources/2022/day18.input"
       io/reader
       line-seq
       (map parse-cube)
       (into #{})))

(defn neighbors [point]
  (into #{}
        (for [i (range (count point))
              offset [-1 1]]
          (update point i + offset))))

(defn run []
  (->> cubes
       (map neighbors)
       (map #(set/difference % cubes))
       (map count)
       (reduce +)))
