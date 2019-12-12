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

(defn gravity [moons [moon-a moon-b] axis]
  (update-in moons
             [moon-a :velocity axis]
             axis-gravity-into-velocity
             (get-in moons [moon-a :position axis])
             (get-in moons [moon-b :position axis])))

(defn move-axis [axis-position axis-velocity]
  (+ axis-position axis-velocity))

(defn move-position [position velocity axis]
  (update position axis move-axis (nth velocity axis)))

(defn move [moon axis]
  (update moon :position move-position (:velocity moon) axis))

(defn moon-pairs [moons]
  (for [moon-a (keys moons)
        moon-b (keys moons)
        :when (not= moon-a moon-b)]
    [moon-a moon-b]))

(defn step [moons axis]
  (->> moons
       moon-pairs
       (reduce #(gravity %1 %2 axis) moons)
       (map-vals #(move % axis))))

(defn converge [moons axis]
  (loop [next-moons (step moons axis)
         steps 1]
    (when (zero? (mod steps 100000))
      (println "Step" steps))
    (if (not= moons next-moons)
      (recur (step next-moons axis) (inc steps))
      steps)))

(defn run []
  [(converge input 0)
   (converge input 1)
   (converge input 2)])