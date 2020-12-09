(ns advent.2020.day9
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]))

(def cypher-len 25)

(defn read-numbers []
  (->> "resources/2020/day9.input"
       io/reader
       line-seq
       (mapv #(Long/parseLong %))))

(defn valid? [numbers n]
  (when (>= n cypher-len)
    (let [target-number (nth numbers n)
          from (- n cypher-len)
          previous-numbers (subvec numbers
                                   from
                                   (+ from cypher-len))]
      (->> (combo/combinations previous-numbers 2)
           (some #(= (reduce + %) target-number))))))

(defn find-invalid-num [numbers]
  (->> (remove (partial valid? numbers)
               (range cypher-len (count numbers)))
       first
       (nth numbers)))

(defn run []
  (let [numbers (read-numbers)]
    (find-invalid-num numbers)))
