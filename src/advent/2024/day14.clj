(ns advent.2024.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def space-width 101)
(def space-height 103)

(def quadrant-nw
  [0 (int (/ space-width 2)) 0 (int (/ space-height 2))])
(def quadrant-ne
  [(inc (int (/ space-width 2))) space-width 0 (int (/ space-height 2))])
(def quadrant-sw
  [0 (int (/ space-width 2)) (inc (int (/ space-height 2))) space-height])
(def quadrant-se
  [(inc (int (/ space-width 2))) space-width (inc (int (/ space-height 2))) space-height])

(defn parse-robot [line]
  (let [[_ px py vx vy] (re-matches #"p=(.+),(.+) v=(.+),(.+)" line)]
    {:p (mapv #(Integer/parseInt %) [px py])
     :v (mapv #(Integer/parseInt %) [vx vy])}))

(def init-robots
  (map parse-robot (-> "resources/2024/day14.input"
                       io/reader
                       slurp
                       (str/split #"\n"))))

(defn next-x [px vx]
  (mod (+ px vx) space-width))

(defn next-y [py vy]
  (mod (+ py vy) space-height))

(defn next-robo-position [{:keys [p v]}]
  {:p [(next-x (first p) (first v))
       (next-y (last p) (last v))]
   :v v})

(defn after-seconds [num-seconds robots]
  (if (zero? num-seconds)
    robots
    (recur (dec num-seconds) (map next-robo-position robots))))

(defn quadrant? [[x y] [fromx tox fromy toy]]
  (and (>= x fromx) (< x tox)
       (>= y fromy) (< y toy)))

(defn quadrant-of [p]
  (cond
    (quadrant? p quadrant-nw)
    :nw

    (quadrant? p quadrant-ne)
    :ne

    (quadrant? p quadrant-sw)
    :sw

    (quadrant? p quadrant-se)
    :se

    :else                                                   ;; middle
    nil))

(defn run []
  (->> init-robots
       (after-seconds 100)
       (map :p)
       (map quadrant-of)
       (remove nil?)
       (group-by identity)
       vals
       (map count)
       (reduce *)))
