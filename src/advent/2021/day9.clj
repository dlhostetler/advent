(ns advent.2021.day9
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def dimensions 2)

(defn pad-coords [coords]
  (->> (repeat (- dimensions (count coords)) 0)
       (concat coords)
       (into [])))

(defn by-coordinates [grid]
  (->> (for [y (range (count grid))
             x (range (-> grid first count))]
         [[x y] (-> grid
                    (nth y)
                    (nth x))])
       (map-keys pad-coords)
       (map-vals str)
       (map-vals #(Integer/parseInt %))
       (into {})))

(defn parse-heightmap []
  (->> "resources/2021/day9.input"
       io/reader
       line-seq
       (mapv vec)
       by-coordinates))

(defn height-at [layout coords]
  (get layout coords -1))

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 inc))

(defn south [position]
  (update position 1 dec))

(defn west [position]
  (update position 0 dec))

(defn low-point? [heightmap [point height]]
  (let [neighbors-heights (->> [(east point)
                        (north point)
                        (south point)
                        (west point)]
                       (map #(height-at heightmap %))
                       (remove #(= -1 %)))]
    (every? #(< height %) neighbors-heights)))

(defn ->low-point [heightmap [point :as position-to-pair]]
  [point (low-point? heightmap position-to-pair)])

(defn run []
  (let [heightmap (parse-heightmap)]
    (->> heightmap
         (map #(->low-point heightmap %))
         (filter last)
         (map first)
         (map heightmap)
         (map inc) ;; risk level
         (reduce +))))
