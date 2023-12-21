(ns advent.2023.day21
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def garden
  (-> "resources/2023/day21.input"
      io/reader
      grid/slurp))

(def start-at (->> garden (filter (comp #(= \S %) last)) first first))

(defn neighbor-plots [point]
  (->> (grid/cardinal-neighbors garden point)
       (filter #(not= \# (garden %)))))

(defn step [plots point num-steps]
  (if (zero? num-steps)
    (swap! plots conj point)
    (doseq [p (neighbor-plots point)]
      (step plots p (dec num-steps)))))

(alter-var-root #'step memoize)

(defn run []
  (let [plots (atom #{})]
    (step plots start-at 64)
    (count @plots)))
