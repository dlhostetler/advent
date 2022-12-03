(ns advent.2022.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (-> "resources/2022/day3.input"
      io/reader
      line-seq))

(defn parse-rucksack [s]
  (-> s seq set))

(defn common-item [rucksack]
  (first (apply set/intersection rucksack)))

(defn priority [c]
  (if (= (str/upper-case c) (str c))
    (-> (- (int c) (int \A))
        inc
        (+ 26))
    (inc (- (int c) (int \a)))))

(defn run []
  (->> input
       (map parse-rucksack)
       (partition-all 3)
       (map common-item)
       (map priority)
       (reduce +)))
