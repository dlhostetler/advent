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

(defn valid-range [numbers target-num from]
  (loop [to from
         sum (nth numbers to)]
    (cond
      ;; this is the proper range
      (= sum target-num)
      [from to]
      ;; ran out of numbers
      (= to (dec (count numbers)))
      nil
      ;; the sum got too big
      (< sum target-num)
      (recur (inc to) (+ sum (nth numbers (inc to))))
      :else
      nil)))

(defn find-sum-range [numbers target-num]
  (->> (map #(valid-range numbers target-num %)
            (range 0 (count numbers)))
       (filter identity)
       first))

(defn run []
  (let [numbers (read-numbers)
        [from to] (find-sum-range numbers (find-invalid-num numbers))
        sum-numbers (subvec numbers from (inc to))]
    (+ (apply min sum-numbers) (apply max sum-numbers))))
