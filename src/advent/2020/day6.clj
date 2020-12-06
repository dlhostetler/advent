(ns advent.2020.day6
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn answer-groups []
  (-> "resources/2020/day6.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn to-all-yes-count [answer-group]
  (->> (str/split answer-group #"\n")
       (map set)
       (apply set/intersection)
       count))

(defn run []
  (->> (answer-groups)
       (map to-all-yes-count)
       (reduce +)))
