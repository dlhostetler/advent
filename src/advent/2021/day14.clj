(ns advent.2021.day14
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def polymer-template
  (->> "NBOKHVHOSVKSSBSVVBCS"
       seq
       (mapv str)))

(defn parse-pair->insertion [s]
  (str/split s #" -> "))

(defn parse-pair [pairStr]
  (->> pairStr
       seq
       (mapv str)))

(defn create-next [[[a :as pair] insertion]]
  [pair [a insertion]])

(def pair->next
  (->> "resources/2021/day14.input"
       io/reader
       line-seq
       (map parse-pair->insertion)
       (map-keys parse-pair)
       (map create-next)
       (into {})))

(defn next-step [polymer]
  (-> (->> polymer
           (partition 2 1)
           (map pair->next)
           flatten
           vec)
      (conj (last polymer))))

(defn max-element [min-element-pair element-pair]
  (if (> (val element-pair) (val min-element-pair))
    element-pair
    min-element-pair))

(defn min-element [min-element-pair element-pair]
  (if (< (val element-pair) (val min-element-pair))
    element-pair
    min-element-pair))

(defn run []
  (let [polymer (->> polymer-template
                     (seq/successive next-step)
                     (drop 10)
                     first)
        element-counts (->> polymer
                            (group-by identity)
                            (map-vals count))
        least-common (reduce min-element element-counts)
        most-common (reduce max-element element-counts)]
    (- (val most-common) (val least-common))))
