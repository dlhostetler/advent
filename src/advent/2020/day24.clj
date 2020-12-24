(ns advent.2020.day24
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def dir->offsets
  {:ne [0 1]
   :e [1 0]
   :se [1, -1]
   :sw [0 -1]
   :w [-1 0]
   :nw [-1 1]})
(def offsets (vals dir->offsets))

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

(defn color-at [tiles position]
  (get tiles position :white))

(defn neighbors [position]
  (map #(next-tile position %) offsets))

(defn neighbor-colors [tiles position]
  (->> position
       neighbors
       (map #(color-at tiles %))))

(defn next-day-tile [tiles position]
  (let [color (color-at tiles position)
        num-black-neighbors (->> position
                                 (neighbor-colors tiles)
                                 (filter (partial = :black))
                                 count)]
    (cond
      ;; Any black tile with zero or more than 2 black tiles immediately
      ;; adjacent to it is flipped to white.
      (and (= color :black)
           (or (zero? num-black-neighbors)
               (> num-black-neighbors 2)))
      :white
      ;; Any white tile with exactly 2 black tiles immediately adjacent to it is
      ;; flipped to black.
      (and (= color :white)
           (= num-black-neighbors 2))
      :black
      ;; Otherwise, the seat's state does not change.
      :else
      color)))

(defn next-day [tiles]
  (->> (for [position (->> tiles
                           keys
                           (mapcat neighbors)
                           (into (-> tiles keys set)))]
         [position (next-day-tile tiles position)])
       (remove #(-> % last (= :white)))
       (into {})))

(defn init-state []
  (->> (tiles)
       (map traverse)
       (reduce flip-at {})))

(defn num-black [tiles]
  (->> tiles
       vals
       (filter (partial = :black))
       count))

(defn run []
  (->> (init-state)
       (seq/successive next-day)
       (take 101)
       last
       num-black))
