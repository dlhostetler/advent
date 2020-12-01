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
   "K)L"
   "K)YOU"
   "I)SAN"])

(def input
  (line-seq (io/reader "resources/2019/day6.input")))

(defn parse-orbit [orbits orbit]
  (let [[orbitee orbiter] (str/split orbit #"\)")]
    (assoc orbits (keyword orbiter) (keyword orbitee))))

(defn parse-orbits [orbits]
  (reduce parse-orbit {} orbits))

(defn direct-orbits [orbits]
  (count orbits))

(defn indirect-orbits [orbits orbiter]
  (loop [orbitee (get orbits orbiter)
         path []]
    (if orbitee
      (recur (get orbits orbitee) (conj path orbitee))
      [orbiter path])))

(defn run []
  (let [o (advent.2019.day6/parse-orbits input)
        orbit-paths (->> (keys o)
                         (map advent.2019.day6/indirect-orbits (repeat o))
                         (into {}))]
    (loop [santa (:SAN orbit-paths)
           you (:YOU orbit-paths)]
      (if (= (last santa) (last you))
        (recur (butlast santa) (butlast you))
        (println (+ (count santa) (count you)))))))