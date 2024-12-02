(ns advent.2024.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-levels [line]
  (->> (str/split line #"\s+")
       (mapv #(Integer/parseInt %))))

(def input
  (mapv parse-levels (-> "resources/2024/day2.input"
                         io/reader
                         slurp
                         (str/split #"\n"))))

(defn ordered? [v]
  (or (= (sort v) v)
      (= (reverse (sort v)) v)))

(defn gradual? [[x y]]
  (let [d (Math/abs (- x y))]
    (<= 1 d 3)))

(defn all-gradual? [v]
  (->> v
       (partition 2 1)
       (every? gradual?)))

(defn safe? [v]
  (and (ordered? v)
       (all-gradual? v)))

(defn run []
  (->> input
       (map safe?)
       (filter true?)
       count))
