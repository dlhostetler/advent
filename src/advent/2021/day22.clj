(ns advent.2021.day22
  (:require [clojure.java.io :as io]
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

(defn x0 [cuboid]
  (-> cuboid :x first))

(defn x1 [cuboid]
  (-> cuboid :x last))

(defn y0 [cuboid]
  (-> cuboid :y first))

(defn y1 [cuboid]
  (-> cuboid :y last))

(defn z0 [cuboid]
  (-> cuboid :z first))

(defn z1 [cuboid]
  (-> cuboid :z last))

(defn cuboid-contains? [a b]
  (and (<= (x0 a) (x0 b)) (>= (x1 a) (x1 b))
       (<= (y0 a) (y0 b)) (>= (y1 a) (y1 b))
       (<= (z0 a) (z0 b)) (>= (z1 a) (z1 b))))

(defn cuboid-intersects? [a b]
  (and (<= (x0 a) (x1 b)) (>= (x1 a) (x0 b))
       (<= (y0 a) (y1 b)) (>= (y1 a) (y0 b))
       (<= (z0 a) (z1 b)) (>= (z1 a) (z0 a))))

(defn volume [{:keys [x y z]}]
  (* (->> x reverse (reduce -))
     (->> y reverse (reduce -))
     (->> z reverse (reduce -))))

(defn in-cuboid? [cuboid v from to]
  (< (from cuboid) v (to cuboid)))

(defn splits [a b from to]
  (let [first-split (from b)
        second-split (to b)]
    (cond-> [(from a)]
            (in-cuboid? a first-split from to) (conj first-split)
            (in-cuboid? a second-split from to) (conj second-split)
            true (conj (to a)))))

(defn chunks [a b from to]
  (->> (splits a b from to)
       (partition 2 1)
       (map vec)))

(defn split [a b]
  (for [x (chunks a b x0 x1)
        y (chunks a b y0 y1)
        z (chunks a b z0 z1)]
    {:x x :y y :z z}))

(defn cuboid-subtract [a b]
  (cond
    (cuboid-contains? b a)
    []
    (not (cuboid-intersects? a b))
    [a]
    :else
    (->> (split a b)
         (remove #(cuboid-contains? b %))
         (into []))))

(defn do-step [cuboids {:keys [on?] :as step}]
  (let [cuboid (dissoc step :on?)]
    (cond-> (->> cuboids
                 (mapcat #(cuboid-subtract % cuboid))
                 (into []))
            on? (conj cuboid))))

(defn run []
  (->> steps
       (reduce do-step [])
       (map volume)
       (reduce +)))
