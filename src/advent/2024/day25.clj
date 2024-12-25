(ns advent.2024.day25
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def grids
  (->> (-> "resources/2024/day25.input"
           io/reader
           slurp
           (str/split #"\n\n"))
       (map #(str/split % #"\n"))
       (map grid/lines->grid)
       (map #(map-vals str %))
       (map #(into {} %))))

(defn key? [grid]
  (->> grid
       keys
       (filter (comp zero? last))
       (map grid)
       (every? (partial = "."))))

(defn to-height [c grid]
  (into []
        (for [x (range 0 (grid/max-x+1 grid))]
          (->> grid
               (filter (comp (partial = x) first first))
               (filter (comp (partial = c) last))
               (map first)
               (map last)
               (reduce max)))))

(def key-heights
  (->> grids
       (filter key?)
       (map (partial to-height "."))))

(defn lock? [grid]
  (->> grid
       keys
       (filter (comp (partial = (grid/max-y grid)) last))
       (map grid)
       (every? (partial = "."))))

(def lock-heights
  (->> grids
       (filter lock?)
       (map (partial to-height "#"))))

(defn fit? [[lock key]]
  (->> (map <= lock key)
       (every? true?)))

(defn run []
  (->> (combo/cartesian-product lock-heights key-heights)
       (map fit?)
       (filter true?)
       count))
