(ns advent.2022.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(declare ordered?)

(defn parse-pair [s]
  (->> (str/split s #"\n")
       (map read-string)))

(def input
  (->> (-> "resources/2022/day13.input"
           io/reader
           slurp
           (str/split #"\n\n"))
       (mapv parse-pair)))

(defn ordered-ints? [left right]
  (cond
    (= left right)
    nil
    (< left right)
    true
    :else
    false))

(defn ordered-lists? [left right]
  (loop [left (if (vector? left) left [left])
         right (if (vector? right) right [right])]
    (let [l (first left)
          r (first right)]
      (cond
        (and (nil? l) (nil? r))
        nil
        (nil? l)
        true
        (nil? r)
        false
        :else
        (let [o? (ordered? [l r])]
          (if (nil? o?)
            (recur (rest left) (rest right))
            o?))))))

(defn ordered?
  ([[left right]]
   (if (or (vector? left) (vector? right))
     (ordered-lists? left right)
     (ordered-ints? left right))))

(defn run []
  #_(->> input
       (drop 1)
       first
       ordered?)
  (->> input
       (map ordered?)
       (map-indexed (fn [i b] [(inc i) b]))
       (filter last)
       (map first)
       (reduce +)))
