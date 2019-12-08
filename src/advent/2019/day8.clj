(ns advent.2019.day8
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/reader "resources/day8.input")))

(defn input->layers [input w h]
  (->> input
       (map #(Integer/parseInt (str %)))
       (partition (* w h))))

(defn num-digit [layer digit]
  (->> layer (filter #(= digit %)) count))

(defn fewest-zeros-layer [layers w h]
  (loop [layers layers
         min-layer nil
         min-zeros (* w h)]
    (if-not (empty? layers)
      (let [num-zeros (num-digit (first layers) 0)]
        (if (< num-zeros min-zeros)
          (recur (rest layers) (first layers) num-zeros)
          (recur (rest layers) min-layer min-zeros)))
      min-layer)))

(defn run []
  (let [layers (input->layers input 25 6)
        layer (fewest-zeros-layer layers 25 6)]
    (* (num-digit layer 1)
       (num-digit layer 2))))