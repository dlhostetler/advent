(ns advent.2022.day24
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [plumbing.core :refer :all]))

(def init-grid
  (->> "resources/2022/day24.input"
       io/reader
       grid/slurp
       (remove (comp (partial = \.) last))
       (map-vals {\> :blizz-east
                  \v :blizz-south
                  \< :blizz-west
                  \^ :blizz-north
                  \# :wall})
       (map-vals (fn [tile] (if (not= tile :wall) [tile] tile)))
       (into {})))

(def start-at
  [1 0])

(def end-at
  [(dec (grid/max-x init-grid)) (grid/max-y init-grid)])

(defn display [grid]
  (grid/print grid
              (fn [tile]
                (cond
                  (and (vector? tile) (= (count tile) 1))
                  (get {:blizz-east ">"
                        :blizz-north "^"
                        :blizz-south "v"
                        :blizz-west "<"} (first tile))
                  (and (vector? tile) (empty? tile))
                  "."
                  (vector? tile)
                  (str (count tile))
                  :else
                  (get {:here "E"
                        :nothing "."
                        :wall "#"} tile)))
              {:default :nothing
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(defn blizzards? [[_ tile]]
  (vector? tile))

(defn valid-destination? [grid to]
  (when (grid/valid-point-or-nil grid to)
    (let [tile (get grid to)]
      (or (nil? tile)
          (and (vector? tile) (empty? tile))))))

(defn remove-blizzard [blizzards blizzard]
  (let [i (.indexOf blizzards blizzard)]
    (into (subvec blizzards 0 i) (subvec blizzards (inc i)))))

(defn wrap-east [grid [_ y :as point]]
  (if (= (get grid point) :wall)
    [(inc (grid/min-x grid)) y]
    point))

(defn wrap-north [grid [x :as point]]
  (if (= (get grid point) :wall)
    [x (dec (grid/max-y grid))]
    point))

(defn wrap-south [grid [x :as point]]
  (if (= (get grid point) :wall)
    [x (inc (grid/min-y grid))]
    point))

(defn wrap-west [grid [_ y :as point]]
  (if (= (get grid point) :wall)
    [(dec (grid/max-x grid)) y]
    point))

(defn move-blizzard [from grid blizzard]
  (let [to (case blizzard
             :blizz-east (wrap-east grid (grid/east from))
             :blizz-north (wrap-north grid (grid/north from))
             :blizz-south (wrap-south grid (grid/south from))
             :blizz-west (wrap-west grid (grid/west from)))]
    (-> grid
        (update from remove-blizzard blizzard)
        (update to (fnil conj []) blizzard))))

(defn move-blizzards [grid [point blizzards]]
  (reduce (partial move-blizzard point) grid blizzards))

(defn move-all-blizzards [grid]
  (reduce move-blizzards grid (filter blizzards? grid)))

(alter-var-root #'move-all-blizzards memoize)

(defn min-path
  ([grid] (min-path 0 [start-at] grid))
  ([path-length froms grid]
   (let [next-grid (move-all-blizzards grid)
         tos (->> (for [from froms
                        dir [:south :east :north :west nil]
                        :let [to (case dir
                                   :east (grid/east from)
                                   :north (grid/north from)
                                   :south (grid/south from)
                                   :west (grid/west from)
                                   nil from)]
                        :when (valid-destination? next-grid to)]
                    to)
                  (into #{}))]
     (if (contains? tos end-at)
       (inc path-length)
       (recur (inc path-length) tos next-grid)))))

(alter-var-root #'min-path memoize)

(defn run []
  (min-path init-grid))
