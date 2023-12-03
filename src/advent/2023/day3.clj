(ns advent.2023.day3
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def input
  (->> "resources/2023/day3.input"
       grid/slurp
       (map-vals str)
       (remove (comp (partial = ".") last))
       (into {})))

(defn gear-point? [point]
  (= (input point) "*"))

(defn digits? [s]
  (and s (every? #(Character/isDigit %) s)))

(defn engine-symbol? [s]
  (and s (not (digits? s))))

(defn part-number-digit? [[point s]]
  (when (digits? s)
    (->> (grid/eight-neighbors input point)
         (map input)
         (map engine-symbol?)
         (some identity)
         boolean)))

(defn start-from [[x0 y]]
  (->> (for [x (reverse (range 0 x0))
             :while (digits? (input [x y]))]
         x)
       reverse
       first))

(defn end-from [[x0 y]]
  (->> (for [x (range (inc x0) (inc (grid/max-x input)))
             :while (digits? (input [x y]))]
         x)
       last))

(defn digit->points [[[x0 y :as point] s]]
  (when-not (digits? s)
    (throw (Exception. (str "not a digit at " point))))
  (let [start-x (or (start-from point) x0)
        end-x (or (end-from point) x0)]
    (into []
          (for [x (range start-x (inc end-x))]
            [x y]))))

(defn points->number [points]
  (->> points
       (map input)
       (apply str)
       Integer/parseInt))

(defn adjacent-gear [point]
  (->> (grid/eight-neighbors input point)
       (filter gear-point?)
       first))

(defn points->gear-point [points]
  (some adjacent-gear points))

(defn into-map [m [gear-point points]]
  (update m gear-point (fnil conj []) points))

(defn gear-4-realz? [[gear-point number-points]]
  (= (count number-points) 2))

(defn run []
  (let [number-points (->> input
                           (filter part-number-digit?)
                           (map digit->points)
                           (into #{}))
        gears->numbers (->> (for [points number-points
                                  :let [gear-point (points->gear-point points)]
                                  :when gear-point]
                              [gear-point (points->number points)])
                            (reduce into-map {})
                            (filter gear-4-realz?)
                            (into {}))]
    (->> gears->numbers
         vals
         (map #(apply * %))
         (reduce +))))
