(ns advent.2024.day3
  (:require [clojure.java.io :as io]))

(def input
  (-> "resources/2024/day3.input"
      io/reader
      slurp))

(def operation-matcher
  (re-matcher #"(do|don't|mul)[(]((\d+),(\d+))?[)]" input))

(defn next-operation []
  (when-let [[_ op _ x y] (re-find operation-matcher)]
    [op
     (when x (Integer/parseInt x))
     (when y (Integer/parseInt y))]))

(def operations
  (->> (repeatedly next-operation)
       (take-while some?)))

(defn run []
    (loop [ops operations
           do? true
           total 0]
      (if (empty? ops)
        total
        (let [[op x y] (first ops)]
          (case op
            "do" (recur (rest ops) true total)
            "don't" (recur (rest ops) false total)
            "mul" (recur (rest ops)
                         do?
                         (if do? (+ total (* x y)) total)))))))
