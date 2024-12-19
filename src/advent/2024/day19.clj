(ns advent.2024.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2024/day19.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(def all-towels
  (->> (str/split (first input) #",")
       (map str/trim)
       (map (fn [line] (->> line seq (mapv str))))
       (into #{})))

(def patterns
  (->> (str/split (last input) #"\n")
       (map (fn [line] (->> line seq (mapv str))))))

(defn towel-match? [towel pattern]
  (and (<= (count towel) (count pattern))
       (= (subvec pattern 0 (count towel)) towel)))

(defn find-towels [n pattern]
  (if (empty? pattern)
    1
    (loop [towels all-towels
           n n]
      (if-let [towel (first towels)]
        (recur (rest towels)
               (if (towel-match? towel pattern)
                 (+ n (find-towels 0 (->> pattern (drop (count towel)) vec)))
                 n))
        n))))

(alter-var-root #'find-towels memoize)

(defn run []
  (->> patterns
       (map (partial find-towels 0))
       (remove zero?)
       count))
