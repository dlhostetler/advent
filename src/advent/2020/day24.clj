(ns advent.2020.day24
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def dir->offsets
  {:ne [0 1]
   :e [1 0]
   :se [1, -1]
   :sw [0 -1]
   :w [-1 0]
   :nw [-1 1]})

(defn parse-directions [line]
  (->> line
       (re-seq #"(n(e|w)|e|s(e|w)|w)")
       (map first)
       (map keyword)))

(defn tiles []
  (->> "resources/2020/day24.input"
       io/reader
       line-seq
       (map parse-directions)))

(defn next-tile [position offsets]
  (mapv + position offsets))

(defn traverse [dirs]
  (->> dirs
       (map dir->offsets)
       (reduce next-tile [0 0])))

(defn flip [color]
  (case color
    :white :black
    :black :white
    :black))

(defn flip-at [tiles position]
  (update tiles position flip))

(defn run []
  (->> (tiles)
       (map traverse)
       (reduce flip-at {})
       vals
       (filter (partial = :black))
       count))
