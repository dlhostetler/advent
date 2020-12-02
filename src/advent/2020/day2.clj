(ns advent.2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader "resources/2020/day2.input")))

(defn parse-rule [rule-str]
  (let [[minmax char] (str/split rule-str #" ")
        [min max] (str/split minmax #"-")]
    {:char char
     :max (Integer/parseInt max)
     :min (Integer/parseInt min)}))

(defn parse-line [line]
  (let [[rule-str password] (str/split line #":")]
    {:password password
     :rule (parse-rule rule-str)}))

(defn is-valid [{:keys [password rule]}]
  (let [{:keys [char max min]} rule
        num-char (->> (seq password)
                      (map str)
                      (filter (partial = char))
                      count)]
    (<= min num-char max)))

(defn run []
  (->> input
       (map parse-line)
       (map is-valid)
       (filter identity)
       count))
