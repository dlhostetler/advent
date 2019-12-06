(ns advent.2019.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-input
  ["COM)B"
   "B)C"
   "C)D"
   "D)E"
   "E)F"
   "B)G"
   "G)H"
   "D)I"
   "E)J"
   "J)K"
   "K)L"])

(def input
  (line-seq (io/reader "resources/day6.input")))

(defn parse-orbit [orbits orbit]
  (let [[orbitee orbiter] (str/split orbit #"\)")]
    (assoc orbits (keyword orbiter) (keyword orbitee))))

(defn parse-orbits [orbits]
  (reduce parse-orbit {} orbits))

(defn direct-orbits [orbits]
  (count orbits))

(defn indirect-orbits [orbits orbiter]
  (loop [orbitee (get orbits orbiter)
         n 0]
    (if orbitee
      (recur (get orbits orbitee) (inc n))
      n)))

(defn run []
  (let [o (advent.2019.day6/parse-orbits input)]
    (+ (advent.2019.day6/direct-orbits o)
       (->> (vals o)
            (map advent.2019.day6/indirect-orbits (repeat o))
            (reduce + 0)))))