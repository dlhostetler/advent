(ns advent.2021.day5
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
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

(defn dir [a b]
  (if (< a b) inc dec))

(defn next-diag [[x y] x-dir y-dir]
  [(x-dir x) (y-dir y)])

(defn infiniline [point x-dir y-dir]
  (seq/successive next-diag point x-dir y-dir))

(defn diag [[x1 y1 :as from] [x2 y2 :as to]]
  (conj (->> (infiniline from
                         (dir x1 x2)
                         (dir y1 y2))
             (take-while #(not= to %))
             vec)
        to))

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
      (diag [x1 y1] [x2 y2]))))

(defn dangerous? [[_ occurrences]]
  (> occurrences 1))

(defn run []
  (->> (input)
       (mapcat coords)
       (frequencies)
       (filter dangerous?)
       count))
