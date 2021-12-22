(ns advent.2021.day22
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [plumbing.core :refer :all]))

(def line-regex #"(on|off) x=(.+)\.\.(.+),y=(.+)\.\.(.+),z=(.+)\.\.(.+)")

(defn parse-line [line]
  (let [[_
         on-or-off
         from-x
         to-x
         from-y
         to-y
         from-z
         to-z] (re-matches line-regex line)]
    {:on? (= on-or-off "on")
     :x [(Integer/parseInt from-x) (inc (Integer/parseInt to-x))]
     :y [(Integer/parseInt from-y) (inc (Integer/parseInt to-y))]
     :z [(Integer/parseInt from-z) (inc (Integer/parseInt to-z))]}))

(def steps
  (->> "resources/2021/day22.input"
       io/reader
       line-seq
       (map parse-line)))

(defn confined-range [from to]
  (range (if (< from -50) -50 from)
         (if (> to 51) 51 to)))

(defn ->cuboid [[from-x to-x] [from-y to-y] [from-z to-z]]
  (->> (for [x (confined-range from-x to-x)
             y (confined-range from-y to-y)
             z (confined-range from-z to-z)]
         [x y z])
       (into #{})))

(defn do-step [cubes {:keys [on? x y z]}]
  (let [cuboid (->cuboid x y z)]
    (if on?
      (set/union cubes cuboid)
      (set/difference cubes cuboid))))

(defn run []
  (->> steps
       (reduce do-step #{})
       count))
