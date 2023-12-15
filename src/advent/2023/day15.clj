(ns advent.2023.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day15.input"
      io/reader
      slurp
      (str/split #",")))

(defn hash-char [h s]
  (-> h
      (+ (int s))
      (* 17)
      (mod 256)))

(defn hash-string [s]
  (reduce hash-char 0 s))

(defn run []
  (->> input
      (map hash-string)
      (reduce +)))
