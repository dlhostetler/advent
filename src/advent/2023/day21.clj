(ns advent.2023.day21
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def garden
  (-> "resources/2023/day21.input"
      io/reader
      grid/slurp))

(def start-at (->> garden (filter (comp #(= \S %) last)) first first))

(defn not-rock? [[x y]]
  (not= \# (garden [(mod x (grid/max-x+1 garden))
                    (mod y (grid/max-y+1 garden))])))

(defn neighbor-plots [point]
  (->> [(grid/north point)
        (grid/east point)
        (grid/south point)
        (grid/west point)]
       (filter not-rock?)))

(defn step [plots point num-steps]
  (if (zero? num-steps)
    (swap! plots conj point)
    (doseq [p (neighbor-plots point)]
      (step plots p (dec num-steps)))))

(alter-var-root #'step memoize)

(defn num-plots [num-steps]
  (let [plots (atom #{})]
    (step plots start-at num-steps)
    (count @plots)))

(defn q [n [a0 a1 a2]]
  (let [b0 a0
        b1 (- a1 a0)
        b2 (- a2 a1)]
    (long (+ b0
             (* b1 n)
             (* (/ (* n (- n 1)) 2)
                (- b2 b1))))))

(defn run []
  (let [step-goal 26501365
        width (grid/max-x+1 garden)
        n (int (mod step-goal width))]
    (q (long (/ step-goal width))
       [(num-plots n)
        (num-plots (+ n width))
        (num-plots (+ n (* 2 width)))])))
