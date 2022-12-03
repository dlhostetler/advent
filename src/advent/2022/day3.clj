(ns advent.2022.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (-> "resources/2022/day3.input"
      io/reader
      line-seq))

(defn parse-rucksack [s]
  (->> (seq s)
       (split-at (-> s count (/ 2)))
       (map set)))

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
       (map common-item)
       (map priority)
       (reduce +)))
