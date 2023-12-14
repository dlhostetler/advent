(ns advent.2023.day14
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def round-rock \O)

(def init-grid
  (->> "resources/2023/day14.input"
       grid/slurp
       (remove (comp (partial = \.) last))
       (into {})))

(def max-y (grid/max-y init-grid))

(defn move-to [grid from]
  (or (->> (grid/points-north grid from)
           (grid/with-tiles grid)
           (take-while (grid/pred-tile= nil))
           last
           first)
      from))

(defn tilt-point [grid point]
  (-> grid
      (dissoc point)
      (assoc (move-to grid point) round-rock)))

(defn tilt-line [grid x]
  (reduce tilt-point
          grid
          (->> (grid/points-col grid x)
               (filter (grid/pred-point->tile= grid round-rock)))))

(defn tilt-north [grid]
  (reduce tilt-line
          grid
          (range (grid/min-x grid) (grid/max-x+1 grid))))

(defn ->load [[[x y] rock]]
  (if (= rock round-rock)
    (inc (- max-y y))
    0))

(defn run []
  (grid/print init-grid identity {:padding 0
                                  :y-dir :top-down})
  (->> (-> init-grid
           tilt-north
           (grid/print-> identity {:padding 0
                                   :y-dir :top-down}))
       (map ->load)
       (reduce +)))
