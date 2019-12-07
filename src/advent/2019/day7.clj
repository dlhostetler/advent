(ns advent.2019.day7
  (:require [advent.2019.intcode :as intcode]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def input
  [3 8 1001 8 10 8 105 1 0 0 21 38 63 88 97 118 199 280 361 442 99999 3 9 1002 9 3 9 101 2 9 9 1002 9 4 9 4 9 99 3 9 101 3 9 9 102 5 9 9 101 3 9 9 1002 9 3 9 101 3 9 9 4 9 99 3 9 1002 9 2 9 1001 9 3 9 102 3 9 9 101 2 9 9 1002 9 4 9 4 9 99 3 9 102 2 9 9 4 9 99 3 9 102 4 9 9 101 5 9 9 102 2 9 9 101 5 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 101 1 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 99])

(def test-input
  [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
   1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

(defn amplifier [memory phase-setting input]
  (-> (str phase-setting "\n" input "\n")
      (with-in-str (intcode/execute-instructions memory))
      with-out-str
      str/trim
      Integer/parseInt))

(defn thruster-signal [memory phase-settings]
  (loop [phase-settings phase-settings
         input 0]
    (if-not (empty? phase-settings)
      (recur (rest phase-settings)
             (amplifier memory (first phase-settings) input))
      input)))

(defn run []
  (let [all-phase-settings (combo/permutations #{0 1 2 3 4})]
    (->> all-phase-settings
         (map thruster-signal (repeat input))
         (apply max))))