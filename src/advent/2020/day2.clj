(ns advent.2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (line-seq (io/reader "resources/2020/day2.input")))

(defn parse-rule [rule-str]
  (let [[firstsecond char] (str/split rule-str #" ")
        [first second] (str/split firstsecond #"-")]
    {:char char
     :first (Integer/parseInt first)
     :second (Integer/parseInt second)}))

(defn parse-line [line]
  (let [[rule-str password] (str/split line #":")]
    {:password (str/trim password)
     :rule (parse-rule rule-str)}))

(defn char-at [s i]
  (str (nth s (dec i))))

(defn is-valid [{:keys [password rule]}]
  (let [{:keys [char first second]} rule
        at-first (= char (char-at password first))
        at-second (= char (char-at password second))]
    (and (or at-first at-second)
         (not (and at-first at-second)))))

(defn run []
  (->> input
       (map parse-line)
       (map is-valid)
       (filter identity)
       count))
