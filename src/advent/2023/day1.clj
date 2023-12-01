(ns advent.2023.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2023/day1.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn digit? [c]
  (Character/isDigit c))

(defn to-number [s]
  (Integer/parseInt (str (->> s (filter digit?) first)
                         (->> s (filter digit?) last))))

(defn run []
  (->> input
       (map to-number)
       (reduce +)))
