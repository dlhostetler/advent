(ns advent.2021.day25
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(defn empty-value? [value]
  (= value "."))

(defn east-tile? [[_ value]]
  (= value ">"))

(defn south-tile? [[_ value]]
  (= value "v"))

(def initial-layout
  (->> "resources/2021/day25.input"
       io/reader
       grid/slurp
       (map-vals str)))

(defn east-from [layout [x y]]
  (let [east-x (inc x)]
    (if (> east-x (grid/max-x layout))
      [0 y]
      [east-x y])))

(defn south-from [layout [x y]]
  (let [south-y (inc y)]
    (if (> south-y (grid/max-y layout))
      [x 0]
      [x south-y])))

(defn move [from-layout dir to-layout [coords cucumber]]
  (let [to (dir from-layout coords)]
    (if (empty-value? (get from-layout to))
      (-> to-layout
          (assoc coords ".")
          (assoc to cucumber))
      (assoc to-layout coords cucumber))))

(defn migrate-east [layout]
  (reduce #(move layout east-from %1 %2)
          layout
          (filter east-tile? layout)))

(defn migrate-south [layout]
  (reduce #(move layout south-from %1 %2)
          layout
          (filter south-tile? layout)))

(defn next-state [layout]
  (-> layout
      migrate-east
      migrate-south))

(defn print-layout [layout]
  (grid/print layout identity {:padding 0
                               :y-dir :top-down}))

(defn find-duplicate
  ([states]
   (find-duplicate 1 states))
  ([state-num states]
   (if (= (first states) (-> states rest first))
     (println state-num)
     (recur (inc state-num) (rest states)))))

(defn run []
  (->> initial-layout
       (seq/successive next-state)
       find-duplicate))
