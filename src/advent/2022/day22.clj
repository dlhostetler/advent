(ns advent.2022.day22
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))
(def input
  (->> "resources/2022/day22.input"
       io/reader
       slurp))

(def grid
  (->> (-> (str/split input #"\n\n")
           first
           (str/split #"\n")
           grid/lines->grid)
       (map-vals {\. :open
                  \# :rock
                  \@ nil
                  \space nil})
       (remove (comp nil? last))
       (into {})))

(defn parse-int [i]
  (try
    (Integer/parseInt i)
    (catch Exception _ i)))

(defn ->path [{heading :heading :as state} next]
  (if (int? next)
    (-> state
        (update :path into (repeat next heading)))
    (-> state
        (update :heading
                (if (= next "R")
                  {:east :south
                   :north :east
                   :south :west
                   :west :north}
                  {:east :north
                   :north :west
                   :south :east
                   :west :south})))))

(def path
  (->> (str/split input #"\n\n")
       last
       (re-seq #"([0-9]+|[LR])")
       (map first)
       (map parse-int)
       (reduce ->path {:heading :east :path []})
       :path))

(defn display-> [grid]
  (grid/print-> grid
                {:here "@"
                 :nothing " "
                 :open "."
                 :rock "#"}
                {:default :nothing
                 :empty-point " "
                 :padding 0
                 :y-dir :top-down}))

(def start-at
  (->> grid
       keys
       (filter (comp zero? last))
       sort
       first))

(defn max-x-at-y [grid y]
  (->> grid
       keys
       (filter (comp (partial = y) last))
       (map first)
       (apply max)))

(alter-var-root #'max-x-at-y memoize)

(defn max-y-at-x [grid x]
  (->> grid
       keys
       (filter (comp (partial = x) first))
       (map last)
       (apply max)))

(alter-var-root #'max-y-at-x memoize)

(defn min-x-at-y [grid y]
  (->> grid
       keys
       (filter (comp (partial = y) last))
       (map first)
       (apply min)))

(alter-var-root #'min-x-at-y memoize)

(defn min-y-at-x [grid x]
  (->> grid
       keys
       (filter (comp (partial = x) first))
       (map last)
       (apply min)))

(alter-var-root #'min-y-at-x memoize)

(defn wrap-x [grid [x y]]
  (let [min-x (min-x-at-y grid y)
        max-x (max-x-at-y grid y)]
    (cond
      (< x min-x)
      [max-x y]

      (> x max-x)
      [min-x y]

      :else
      [x y])))

(alter-var-root #'wrap-x memoize)

(defn wrap-y [grid [x y]]
  (let [min-y (min-y-at-x grid x)
        max-y (max-y-at-x grid x)]
    (cond
      (< y min-y)
      [x max-y]

      (> y max-y)
      [x min-y]

      :else
      [x y])))

(alter-var-root #'wrap-y memoize)

(defn walk-path [grid from dir]
  #_(println "move" dir "from" from)
  (let [to (case dir
             :east (wrap-x grid (grid/east from))
             :north (wrap-y grid (grid/north from))
             :south (wrap-y grid (grid/south from))
             :west (wrap-x grid (grid/west from)))]
    (if (= (get grid to) :rock)
      from
      to)))

(defn password [[x y]]
  (+ (* (inc y) 1000)
     (* (inc x) 4)
     (->> path
          last
          {:east 0
           :north 3
           :south 1
           :west 2})))

(defn run []
  (->> path
       (reduce (partial walk-path grid) start-at)
       #_display->
       password))
