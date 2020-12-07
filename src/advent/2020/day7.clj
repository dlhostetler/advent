(ns advent.2020.day7
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn parse-target [target-str]
  (let [[num color] (rest (re-matches #"(\d+) (.+) bags?" target-str))]
    (when num
      {:color color
       :num (Integer/parseInt num)})))

(defn parse-rule [line]
  (let [[k vs] (-> (re-matches #"(.+) bags contain (.+ bags?,?)+." line)
                   rest)
        targets (->> (str/split vs #",")
                     (map str/trim)
                     (map parse-target)
                     (into #{}))]
    [k (when (first targets) targets)]))

(defn slurp-rules []
  (->> "resources/2020/day7.input"
       io/reader
       line-seq
       (map parse-rule)
       (into {})))

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

(defn count-bags [rules color]
  (if-let [targets (get rules color)]
    (->> targets
         (map (fn [target]
                (* (:num target)
                   (count-bags rules (:color target)))))
         (reduce + 1))
    1))

(defn run []
  (let [rules (slurp-rules)]
    (count-bags rules "shiny gold")))
