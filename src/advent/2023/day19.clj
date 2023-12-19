(ns advent.2023.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day19.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn parse-rule [rule-str]
  (let [[_ _ category condition value result]
        (re-matches #"(([a-z]+)?(<|>)?(\d+)?:)?(.+)" rule-str)]
    {:condition condition
     :category category
     :result result
     :value (when value (Integer/parseInt value))}))

(defn parse-workflow [line]
  (let [[_ name rules-str] (re-matches #"(.+)\{(.+)\}" line)
        rules (str/split rules-str #",")]
    [name (map parse-rule rules)]))

(def workflows
  (->> (-> input
           first
           (str/split #"\n"))
       (map parse-workflow)
       (into {})))

(declare apply-workflow)

(defn category->n [[from to]]
  (inc (- to from)))

(defn rating->n [rating]
  (->> rating
       vals
       (map category->n)
       (reduce *)))

(defn do-result [rating result]
  (case result
    "A" (rating->n rating)
    "R" 0
    (apply-workflow rating result)))

(defn split-rating-< [rating category value]
  (let [[from to] (get rating category)]
    (cond
      (< to value)
      [(assoc rating category [from to])
       nil]

      (< value from)
      [nil
       (assoc rating category [from to])]

      :else
      [(assoc rating category [from (dec value)])
       (assoc rating category [value to])])))

(defn split-rating-> [rating category value]
  (let [[from to] (get rating category)]
    (cond
      (> from value)
      [(assoc rating category [from to])
       nil]

      (< to value)
      [nil
       (assoc rating category [from to])]

      :else
      [(assoc rating category [(inc value) to])
       (assoc rating category [from value])])))

(defn split-rating [rating {:keys [category condition value]}]
  (if (= condition "<")
    (split-rating-< rating category value)
    (split-rating-> rating category value)))

(defn apply-rules [rating rules]
  (let [rule (first rules)]
    (if (:condition rule)
      (let [[pass-rating fail-rating] (split-rating rating rule)]
        (+ (if pass-rating (do-result pass-rating (:result rule)) 0)
           (if fail-rating (apply-rules fail-rating (rest rules)) 0)))
      (do-result rating (:result rule)))))

(defn apply-workflow [rating wf-name]
  (apply-rules rating (get workflows wf-name)))

(defn run []
  (apply-workflow {"x" [1 4000]
                   "m" [1 4000]
                   "a" [1 4000]
                   "s" [1 4000]}
                  "in"))
