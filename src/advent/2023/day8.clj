(ns advent.2023.day8
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day8.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(def all-instructions (->> input
                           first
                           (map str)
                           (map keyword)
                           cycle))

(defn parse-node [line]
  (let [[_ node left right] (->> (re-matches #"(.+) = \((.+), (.+)\)" line))]
    [[[node :L] left]
     [[node :R] right]]))

(def graph (->> (str/split (second input) #"\n")
                (map parse-node)
                (apply concat)
                (into {})))

(defn next-node [{at-node :at-node steps :steps}]
  (let [next-instruction (nth all-instructions steps)]
    {:at-node (get graph [at-node next-instruction])
     :steps (inc steps)}))

(defn not-at-zzz? [{at-node :at-node}]
  (not= "ZZZ" at-node))

(defn run []
  (->> {:at-node "AAA"
        :steps 0}
       (seq/successive next-node)
       (take-while not-at-zzz?)
       count))
