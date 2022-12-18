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

(def min-x (->> cubes (map #(nth % 0)) (reduce min) dec))
(def max-x (->> cubes (map #(nth % 0)) (reduce max) inc))
(def min-y (->> cubes (map #(nth % 1)) (reduce min) dec))
(def max-y (->> cubes (map #(nth % 1)) (reduce max) inc))
(def min-z (->> cubes (map #(nth % 2)) (reduce min) dec))
(def max-z (->> cubes (map #(nth % 2)) (reduce max) inc))

(defn in-bounds? [[x y z]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)
       (<= min-z z max-z)))

(defn valid-neighbors [point]
  (->> point
       neighbors
       (filter in-bounds?)
       (remove cubes)
       (into #{})))

(defn count-external-surfaces
  ([] (count-external-surfaces 0 #{} (valid-neighbors [min-x min-y min-z])))
  ([num-surfaces checked to-check]
   (if (empty? to-check)
     num-surfaces
     (let [point (first to-check)
           checked (conj checked point)]
       (recur (+ num-surfaces (->> point neighbors (filter cubes) count))
              checked
              (-> to-check
                  (disj point)
                  (into (->> point valid-neighbors (remove checked)))))))))

(defn run []
  (->> cubes
       (map neighbors)
       (map #(set/difference % cubes))
       (map count)
       (reduce +)))
