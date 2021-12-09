(ns advent.2021.day9
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
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

(defn height-at [heightmap coords]
  (get heightmap coords -1))

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 inc))

(defn south [position]
  (update position 1 dec))

(defn west [position]
  (update position 0 dec))

(defn heightmap-max-x [heightmap]
  (->> heightmap
       keys
       (map first)
       (apply max)))

(alter-var-root #'heightmap-max-x memoize)

(defn heightmap-max-y [heightmap]
  (->> heightmap
       keys
       (map last)
       (apply max)))

(alter-var-root #'heightmap-max-y memoize)

(defn invalid-point? [heightmap [x y]]
  (or (< x 0)
      (> x (heightmap-max-x heightmap))
      (< y 0)
      (> y (heightmap-max-y heightmap))))

(defn neighbors [heightmap point]
  (->> [(east point)
        (north point)
        (south point)
        (west point)]
       (remove #(invalid-point? heightmap %))
       (into #{})))

(defn low-point? [heightmap [point height]]
  (let [neighbors-heights (->> (neighbors heightmap point)
                               (map #(height-at heightmap %)))]
    (every? #(< height %) neighbors-heights)))

(defn ->low-point [heightmap [point :as position-to-pair]]
  [point (low-point? heightmap position-to-pair)])

(defn unvisited-neighbors [heightmap visited point]
  (set/difference (neighbors heightmap point) visited))

(defn next-to-visit [heightmap to-visit visited point]
  (let [to-visit (disj to-visit point)]
    (if (= (height-at heightmap point) 9)
      to-visit
      (into to-visit (unvisited-neighbors heightmap visited point)))))

(defn next-basin-size [heightmap basin-size point]
  (if (= (height-at heightmap point) 9)
    basin-size
    (inc basin-size)))

(defn ->basin-size [heightmap from-point]
  (loop [to-visit #{from-point}
         visited #{}
         basin-size 0]
    (if-not (empty? to-visit)
      (let [next-point (first to-visit)]
        (recur (next-to-visit heightmap to-visit visited next-point)
               (conj visited next-point)
               (next-basin-size heightmap basin-size next-point)))
      basin-size)))

(defn run []
  (let [heightmap (parse-heightmap)
        low-points (->> heightmap
                        (map #(->low-point heightmap %))
                        (filter last)
                        (map first))]
    (->> low-points
         (map #(->basin-size heightmap %))
         sort
         reverse
         (take 3)
         (reduce *))))
