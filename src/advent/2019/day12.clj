(ns advent.2019.day12
  (:require [plumbing.core :refer :all]))

(def test-input
  {:io {:position [-1 0 2]
        :velocity [0 0 0]}
   :europa {:position [2 -10 -7]
            :velocity [0 0 0]}
   :ganymede {:position [4 -8 8]
              :velocity [0 0 0]}
   :callisto {:position [3 5 -1]
              :velocity [0 0 0]}})

(def input
  {:io {:position [16 -11 2]
        :velocity [0 0 0]}
   :europa {:position [0 -4 7]
            :velocity [0 0 0]}
   :ganymede {:position [6 4 -10]
              :velocity [0 0 0]}
   :callisto {:position [-3 -2 -4]
              :velocity [0 0 0]}})

(defn axis-gravity-into-velocity [axis-velocity axis-position-a axis-position-b]
  (+ axis-velocity (compare axis-position-b axis-position-a)))

(defn gravity-into-velocity [velocity-a position-a position-b]
  (mapv axis-gravity-into-velocity velocity-a position-a position-b))

(defn gravity [moons [moon-a moon-b]]
  (update-in moons
             [moon-a :velocity]
             gravity-into-velocity
             (get-in moons [moon-a :position])
             (get-in moons [moon-b :position])))

(defn move-axis [axis-position axis-velocity]
  (+ axis-position axis-velocity))

(defn move-position [position velocity]
  (map move-axis position velocity))

(defn move [moon]
  (update moon :position move-position (:velocity moon)))

(defn moon-pairs [moons]
  (for [moon-a (keys moons)
        moon-b (keys moons)
        :when (not= moon-a moon-b)]
    [moon-a moon-b]))

(defn step [moons]
  (->> moons
       moon-pairs
       (reduce gravity moons)
       (map-vals move)))

(defn steps [moons n]
  (loop [moons moons
         steps n]
    (if (pos? steps)
      (recur (step moons) (dec steps))
      moons)))

(defn potential [moon]
  (->> moon
       :position
       (map #(Math/abs %))
       (reduce + 0)))

(defn kinetic [moon]
  (->> moon
       :velocity
       (map #(Math/abs %))
       (reduce + 0)))

(defn run []
  (let [moons (steps input 1000)
        pot (mapv potential (vals moons))
        kin (mapv kinetic (vals moons))
        total (map #(* %1 %2) pot kin)]
    (reduce + 0 total)))