(ns advent.2020.day11
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def floor-tile \.)
(def unoccupied-tile \L)
(def occupied-tile \#)

(defn parse-layout []
  (->> "resources/2020/day11.input"
       io/reader
       line-seq
       (mapv vec)))

(defn seat-at [seats row col]
  (-> seats (nth row) (nth col)))

(defn floor? [seat]
  (= seat floor-tile))

(defn occupied? [seat]
  (= seat occupied-tile))

(defn valid-row? [layout [row col]]
  (and (>= row 0)
       (< row (count layout))))

(defn valid-col? [layout [row col]]
  (and (>= col 0)
       (< col (-> layout first count))))

(defn neighbor-coords [layout row col]
  (->> (-> (combo/cartesian-product [-1 0 1] [-1 0 1])
           set
           (disj [0 0]))
       (map (fn [[offset-y offset-x]]
              [(+ row offset-y) (+ col offset-x)]))
       (filter #(valid-row? layout %))
       (filter #(valid-col? layout %))))

(defn build-neighbors [layout]
  (->> (for [row (range (count layout))
             col (range (-> layout first count))]
         [[row col] (neighbor-coords layout row col)])
       (into {})))

(defn next-seat [layout neighbors row col]
  (let [seat (seat-at layout row col)
        num-neighbors (->> (get neighbors [row col])
                           (map #(apply seat-at layout %))
                           (filter occupied?)
                           count)]
    (cond
      ;; If a seat is empty (L) and there are no occupied seats adjacent to it,
      ;; the seat becomes occupied.
      (and (not (occupied? seat))
           (zero? num-neighbors))
      occupied-tile
      ;; If a seat is occupied (#) and four or more seats adjacent to it are
      ;; also occupied, the seat becomes empty.
      (and (occupied? seat)
           (>= num-neighbors 4))
      unoccupied-tile
      ;; Otherwise, the seat's state does not change.
      :else
      seat)))

(defn next-layout [layout neighbors]
  (for [row (range (count layout))]
    (for [col (range (-> layout first count))
          :let [tile (seat-at layout row col)]]
      (if (floor? tile)
        tile
        (next-seat layout neighbors row col)))))

(defn all-states [layout neighbors]
  (lazy-seq
    (cons layout
          (all-states (next-layout layout neighbors) neighbors))))

(defn occupado [layout]
  (->> layout
       flatten
       (filter occupied?)
       count))

(defn first-duplicate [coll]
  (->> (map vector coll (rest coll))
       (filter #(apply = %))
       first
       first))

(defn run []
  (let [layout (parse-layout)
        neighbors (build-neighbors layout)]
    (->> (all-states layout neighbors)
         first-duplicate
         occupado)))
