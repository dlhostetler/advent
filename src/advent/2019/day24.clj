(ns advent.2019.day24
  (:require [advent.2019.grid :as grid]
            [clojure.java.io :as io]))

(def width 5)
(def height 5)

(def all-points
  (into [] (for [x (range width)
                 y (range height)]
             [x y])))

(defn parse-bug [x y c]
  (when (= \# c)
    [x y]))

(defn parse-row [y row]
  (map-indexed #(parse-bug %1 y %2) row))

(defn parse-area []
  (->> "resources/day24.input"
       io/reader
       line-seq
       (map seq)
       (map-indexed parse-row)
       (mapcat identity)
       (remove nil?)
       (into #{})))

(defn display [bugs]
  (let [grid (into {} (zipmap bugs (repeat "#")))]
    (grid/print grid
                (fn [tile]
                  (when tile tile))
                {:default "."
                 :empty-point "."
                 :max-x (dec width)
                 :max-y (dec height)
                 :min-x 0
                 :min-y 0
                 :padding 0
                 :y-dir :top-down})
    (println))
  bugs)

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 dec))

(defn south [position]
  (update position 1 inc))

(defn west [position]
  (update position 0 dec))

(defn point->neighbors [point]
  [(east point)
   (north point)
   (south point)
   (west point)])

(defn life [bugs next-bugs point]
  (let [bug? (bugs point)
        neighbor-bugs (->> point
                           point->neighbors
                           (filter bugs))
        num-neighbor-bugs (count neighbor-bugs)]
    (cond
      (and bug? (= 1 num-neighbor-bugs))
      (conj next-bugs point)
      (and (not bug?)
           (or (= 1 num-neighbor-bugs)
               (= 2 num-neighbor-bugs)))
      (conj next-bugs point)
      :else
      next-bugs)))

(defn minute [bugs]
  (reduce #(life bugs %1 %2) #{} all-points))

(defn point->biodiversity [[x y]]
  (let [n (+ (* y height) x)]
    (long (Math/pow 2 n))))

(defn run []
  (->> (loop [bugs (parse-area)
              seen #{}]
         (if-not (seen bugs)
           (do
             (display bugs)
             (recur (minute bugs)
                    (conj seen bugs)))
           bugs))
       display
       (map point->biodiversity)
       (reduce + 0)))