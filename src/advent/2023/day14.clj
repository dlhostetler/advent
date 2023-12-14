(ns advent.2023.day14
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
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

(alter-var-root #'tilt-north memoize)

(defn tilt-east [grid]
  (-> grid
      grid/rotate-90-neg
      tilt-north
      grid/rotate-90-pos))

(defn tilt-south [grid]
  (-> grid
      grid/rotate-90-pos
      grid/rotate-90-pos
      tilt-north
      grid/rotate-90-neg
      grid/rotate-90-neg))

(defn tilt-west [grid]
  (-> grid
      grid/rotate-90-pos
      tilt-north
      grid/rotate-90-neg))

(defn spin-cycle [grid]
  (-> grid
      tilt-north
      tilt-west
      tilt-south
      tilt-east))

(alter-var-root #'spin-cycle memoize)

(defn ->load [[[x y] rock]]
  (if (= rock round-rock)
    (inc (- max-y y))
    0))

(defn total-load [grid]
  (->> grid
       (map ->load)
       (reduce +)))

(defn find-cycle
  ([loads]
   (find-cycle loads 0 5))
  ([loads start n]
   (println (str "start=" start) (str "n=" n))
   (if (> start 1000)
     (do
       (println "got to" (str "start=" start))
       nil)
     (let [p0 (->> loads (drop start) (take n))
           p1 (->> loads (drop start) (drop n) (take n))]
       (cond
         (> n 100)
         (do
           (println "got to" (str "start=" start) "max partition")
           (recur loads (inc start) 5))

         (= p0 p1)
         {:start start
          :cycle p0}

         :else
         (recur loads start (inc n)))))))

(defn cycle->load [cycle-results]
  (let [idx (mod (- 1000000000 (:start cycle-results))
                 (-> cycle-results :cycle count))]
    (nth (:cycle cycle-results) idx)))

(defn run []
  (->> init-grid
       (seq/successive spin-cycle)
       (map total-load)
       find-cycle
       cycle->load))
