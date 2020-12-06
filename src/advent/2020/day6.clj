(ns advent.2020.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn answer-groups []
  (-> "resources/2020/day6.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn to-yes-count [answer-group]
  (-> answer-group
      (str/replace "\n" "")
      set
      count))

(defn run []
  (->> (answer-groups)
       (map to-yes-count)
       (reduce +)))
