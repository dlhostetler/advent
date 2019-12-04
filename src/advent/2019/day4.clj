(ns advent.2019.day4
  (:require [clojure.string :as str]))

(defn adjacent-pair? [pairs]
  (->> pairs
       (map set)
       (some #(= 1 (count %)))))

(defn vaguely-increasing? [[a b]]
  (<= a b))

(defn password? [^String s]
  (let [pairs (->> s
                   seq
                   (map str)
                   (map #(Integer/parseInt %))
                   (partition 2 1))]
    (and (= 6 (count s))
         (adjacent-pair? pairs)
         (every? vaguely-increasing? pairs))))

(defn run []
  (->> (range 240298 784957)
       (map str)
       (filter password?)
       count))