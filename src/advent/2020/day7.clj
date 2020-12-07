(ns advent.2020.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-target [target-str]
  (let [[num color] (rest (re-matches #"(\d+) (.+) bags?" target-str))]
    {:color color
     :num (when num (Integer/parseInt num))}))

(defn parse-rule [line]
  (let [[k vs] (-> (re-matches #"(.+) bags contain (.+ bags?,?)+." line)
                   rest)]
    [k (->> (str/split vs #",")
            (map str/trim)
            (map parse-target)
            (into #{}))]))

(defn slurp-rules []
  (->> "resources/2020/day7.input"
       io/reader
       line-seq
       (map parse-rule)))

(defn targets-contain [[_ targets] color]
  (some #(= (:color %) color) targets))

(defn parents-of [rules color]
  (filter #(targets-contain % color) rules))

(defn all-parents [rules color]
  (loop [next-colors [color]
         parents #{}]
    (let [p (->> (mapcat #(parents-of rules %) next-colors)
                 (map first))]
      (if (empty? p)
        parents
        (recur p (into parents p))))))

(defn run []
  (-> (slurp-rules)
      (all-parents "shiny gold")
      count))
