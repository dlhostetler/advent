(ns advent.2021.day20
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2021/day20.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(def enh-alg
  (-> input first (str/replace #"\n" "")))

(def input-image
  (-> input
      last
      (str/split #"\n")
      grid/lines->grid))

(defn nine-points [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y] [x y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn point->bit [image default-pixel point]
  (get {\. 0 \# 1}
       (get image point default-pixel)))

(defn points->binary [image default-pixel points]
  (->> points
       (map #(point->bit image default-pixel %))
       (apply str)))

(defn binary->int [binary]
  (Integer/parseInt binary 2))

(defn point->pixel [image point default-pixel]
  (->> point
       nine-points
       (points->binary image default-pixel)
       binary->int
       (nth enh-alg)))

(defn next-image* [image default-pixel]
  (->> (for [x (range (- (grid/grid->min-x image) 1)
                      (+ (grid/grid->max-x image) 2))
             y (range (- (grid/grid->min-y image) 1)
                      (+ (grid/grid->max-y image) 2))
             :let [point [x y]]]
         [point (point->pixel image point default-pixel)])
       (into {})))

(defn iteration-default-pixel [iteration]
  (if (and (= (first enh-alg) \#)
           (= (mod iteration 2) 1))
    \#
    \.))

(defn light-pixel? [[_ pixel]]
  (= pixel \#))

(defn next-image [image iteration]
  (let [default-pixel (iteration-default-pixel iteration)]
    (next-image* image default-pixel)))

(defn enhance [image iterations]
  (println "iteration" iterations)
  (if (pos? iterations)
    (recur (next-image image iterations)
           (dec iterations))
    image))

(defn run []
  (->> (-> input-image
           (enhance 50)
           #_(grid/print-> str {:y-dir :top-down}))
       (filter light-pixel?)
       count))
