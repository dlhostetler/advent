(ns advent.2021.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn input []
  (-> "resources/2021/day5.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn h-line [y x1 x2]
  (let [[x1 x2] (sort [x1 x2])]
    (into [] (for [x (range x1 (inc x2))]
               [x y]))))

(defn v-line [x y1 y2]
  (let [[y1 y2] (sort [y1 y2])]
    (into [] (for [y (range y1 (inc y2))]
               [x y]))))

(defn coords [s]
  (let [[from to] (str/split s #" -> ")
        [x1, y1] (mapv #(Integer/parseInt %) (str/split from #","))
        [x2, y2] (mapv #(Integer/parseInt %) (str/split to #","))]
    (cond
      (= x1 x2)
      (v-line x1 y1 y2)
      (= y1 y2)
      (h-line y1 x1 x2)
      :else
      [])))

(defn dangerous? [[_ occurrences]]
  (> occurrences 1))

(defn run []
  (->> (input)
       (mapcat coords)
       (frequencies)
       (filter dangerous?)
       count))
