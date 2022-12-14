(ns advent.2022.day14
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def sand-source [500, 0])

(defn parse-point [s]
  (->> (str/split s #",")
       (mapv #(Integer/parseInt %))))

(defn expand-part [[[from-x from-y] [to-x to-y]]]
  (for [x (range (min from-x to-x) (inc (max from-x to-x)))
        y (range (min from-y to-y) (inc (max from-y to-y)))]
    [[x y] :rock]))

(defn parse-path [line]
  (->> (str/split line #" -> ")
       (map parse-point)
       (partition 2 1)
       (mapcat expand-part)))

(def init-grid
  (->> "resources/2022/day14.input"
       io/reader
       line-seq
       (mapcat parse-path)
       (into {})))

(def abyss-y (grid/max-y init-grid))

(def init-state
  {:falling-at sand-source
   :grid init-grid})

(defn display [state]
  (grid/print (:grid state)
              {:air "."
               :rock "#"
               :sand "o"}
              {:default :air
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(defn next-state [{falling-at :falling-at grid :grid :as state}]
  (cond
    ;; move down
    (nil? (grid (grid/south falling-at)))
    (assoc state :falling-at (grid/south falling-at))
    ;; move down-left
    (nil? (grid (grid/southwest falling-at)))
    (assoc state :falling-at (grid/southwest falling-at))
    ;; move down-right
    (nil? (grid (grid/southeast falling-at)))
    (assoc state :falling-at (grid/southeast falling-at))
    ;; stop
    (= falling-at sand-source)
    (-> state
        (assoc-in [:grid falling-at] :sand)
        (assoc :falling-at nil))
    ;; new sand
    :else
    (-> state
        (assoc-in [:grid falling-at] :sand)
        (assoc :falling-at sand-source))))

(defn falling-into-abyss? [{falling-at :falling-at}]
  (> (last falling-at) abyss-y))

(defn run []
  (->> init-state
       (seq/successive next-state)
       (take-while (comp not falling-into-abyss?))
       last
       :grid
       (map last)
       (filter #(= :sand %))
       count))
