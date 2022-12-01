(ns advent.2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2022/day1.input"
      io/reader
      slurp))

(defn parse-inventory [s]
  (->> (str/split s #"\n")
      (map #(Integer/parseInt %))))

(defn run []
  (->> (str/split input #"\n\n")
       (map parse-inventory)
       (map #(reduce + %))
       sort
       reverse
       (take 3)
       (reduce +)))
