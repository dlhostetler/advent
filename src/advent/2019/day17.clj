(ns advent.2019.day17
  (:require [advent.2019.intcode :as intcode]
            [advent.2019.grid :as grid]))

(defn all-10s? [s]
  (every? #(= 10 %) s))

(defn output->grid [output]
  (loop [rows (->> output
                   (partition-by #(= 10 %))
                   (remove all-10s?))
         grid {}
         y 0]
    (if-not (empty? rows)
      (recur (rest rows)
             (reduce (fn [g [x ascii]]
                       (assoc g [x y] ascii))
                     grid
                     (map-indexed (fn [i ascii] [i ascii]) (first rows)))
             (inc y))
      grid)))

(defn visualize-ascii [ascii]
  (str (char ascii)))

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 inc))

(defn south [position]
  (update position 1 dec))

(defn west [position]
  (update position 0 dec))

(defn scaffold? [ascii]
  (when ascii (= 35 ascii)))

(defn intersection? [grid position]
  (let [adjoining [(east position)
                   (north position)
                   (south position)
                   (west position)]
        num-scaffolds (->> adjoining
                           (map grid)
                           (filter scaffold?)
                           count)]
    (>= num-scaffolds 3)))

(defn run []
  (let [memory (intcode/file->memory "resources/day17.input")
        output (atom [])]
    (intcode/execute-instructions :robot
                                  memory
                                  intcode/stdin
                                  #(swap! output conj %)
                                  nil)
    (let [grid (output->grid @output)
          intersections (->> grid
                             (filter (comp scaffold? val))
                             keys
                             (filter #(intersection? grid %)))]
      (grid/print (reduce (fn [g point] (assoc g point 79)) grid intersections)
                  visualize-ascii
                  {:default 124
                   :empty-point "?"
                   :padding 0
                   :y-dir :top-down})
      (reduce (fn [acc [x y]] (+ acc (* x y))) 0 intersections))))
