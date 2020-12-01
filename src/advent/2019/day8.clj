(ns advent.2019.day8
  (:require [clojure.java.io :as io]))

(def input
  (slurp (io/reader "resources/2019/day8.input")))

(defn input->layers [input w h]
  (->> input
       (map #(Integer/parseInt (str %)))
       (partition w)
       (partition h)))

(defn layer->pixel [layer row col]
  (nth (nth layer row) col))

(defn layers->pixel [layers row col]
  (loop [layers layers]
    (when (seq layers)
      (let [pixel (layer->pixel (first layers) row col)]
        (if (not= 2 pixel)
          pixel
          (recur (rest layers)))))))

(defn run []
  (let [w 25
        h 6
        layers (input->layers input w h)]
    (doseq [row (range h)]
      (doseq [col (range w)]
        (let [pixel (layers->pixel layers row col)]
          (case pixel
            0
            (print " ")
            1
            (print "*"))))
      (println))))