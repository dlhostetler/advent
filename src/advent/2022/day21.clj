(ns advent.2022.day21
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def operator->fn
  {"+" +
   "-" -
   "*" *
   "/" /})

(defn parse-monkey [line]
  (let [[_ name num-or-operand0 operator operand1]
        (re-matches #"(.+): ([a-z0-9]+) ?(.)? ?(.+)?" line)]
    (if operator
      {:name name
       :fn (operator->fn operator)
       :operand0 num-or-operand0
       :operand1 operand1}
      {:name name
       :num (Integer/parseInt num-or-operand0)})))

(def monkeys
  (->> "resources/2022/day21.input"
       io/reader
       line-seq
       (map parse-monkey)))

(defn ->promises []
  (into {}
        (for [{name :name} monkeys]
          [name (promise)])))

(defn do-monkey [promises {name :name :as monkey}]
  (let [p (get promises name)]
    (if (:num monkey)
      (deliver p (:num monkey))
      (deliver p ((:fn monkey)
                  (-> promises (get (:operand0 monkey)) deref)
                  (-> promises (get (:operand1 monkey)) deref))))))

(defn run []
  (let [promises (->promises)]
    (doseq [m monkeys]
      (future (do-monkey promises m)))
    (-> promises (get "root") deref)))
