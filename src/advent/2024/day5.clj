(ns advent.2024.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-str
  (-> "resources/2024/day5.input"
      io/reader
      slurp))

(defn parse-page-ordering-rule [s]
  (->> (str/split s #"\|")
       (mapv #(Integer/parseInt %))))

(defn into-map-of-sets [m [before after]]
  (update m before (fnil conj #{}) after))

(def page-ordering-rules
  (->> (-> input-str
           (str/split #"\n\n")
           first
           (str/split #"\n"))
       (map parse-page-ordering-rule)
       (reduce into-map-of-sets {})))

(defn parse-page-number-update [s]
  (->> (str/split s #",")
       (mapv #(Integer/parseInt %))))

(def page-number-updates
  (->> (-> input-str
           (str/split #"\n\n")
           last
           (str/split #"\n"))
       (map parse-page-number-update)))

(defn page-comp [x y]
  (if (contains? (page-ordering-rules x) y)
    -1
    0))

(defn in-order? [page-number-update]
  (= page-number-update (sort page-comp page-number-update)))

(defn middle-number [page-number-update]
  (get page-number-update (int (/ (count page-number-update) 2))))

(defn run []
  (->> page-number-updates
       (filter in-order?)
       (map middle-number)
       (reduce +)))
