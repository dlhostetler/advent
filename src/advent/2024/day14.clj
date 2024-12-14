(ns advent.2024.day14
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def space-width 101)
(def space-height 103)

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

(defn print-robots [robots]
  (grid/print (zipmap (map :p robots) (repeat "#"))))

(defn distance-between [[robot0 robot1]]
  (+ (Math/abs (- (-> robot0 :p first) (-> robot1 :p first)))
     (Math/abs (- (-> robot0 :p last) (-> robot1 :p last)))))

(defn distance [robots]
  (->> robots
       (partition 2 1)
       (map distance-between)
       (reduce +)))

(defn find-christmas-tree [max-seconds robots]
  (loop [robots robots
         min-distance (distance robots)
         seconds 0]
    (if (>= seconds max-seconds)
      (println "too many seconds")
      (let [d (distance robots)]
        (when (< d min-distance)
          (println seconds)
          (print-robots robots))
        (recur (map next-robo-position robots)
               (min min-distance d)
               (inc seconds))))))

(defn run []
  (find-christmas-tree 10000 init-robots))
