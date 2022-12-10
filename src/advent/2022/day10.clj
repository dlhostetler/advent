(ns advent.2022.day10
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-instruction [s]
  (let [[instruction amount] (str/split s #" ")]
    (-> [nil]
        (?> amount (into [(Integer/parseInt amount)])))))

(def instructions
  (->> "resources/2022/day10.input"
       io/reader
       line-seq
       (mapcat parse-instruction)
       (into [])))

(def init-state
  {:cycle-num 1
   :instructions instructions
   :signal-strength 0
   :x 1})

(defn next-signal-strength [state]
  (assoc state :signal-strength (* (:cycle-num state)
                                   (:x state))))

(defn next-cycle* [state]
  (let [amount (-> state :instructions first)]
    (-> state
        (update :cycle-num inc)
        (update :instructions rest)
        (?> amount (update :x + amount))
        next-signal-strength)))

(defn next-cycle [state]
  (when (not (empty? (:instructions state)))
    (next-cycle* state)))

(defn signal-strength-at [n cycles]
  (->> cycles
       (drop (dec n))
       first
       :signal-strength))

(defn run []
  (let [cycles (->> init-state
                    (seq/successive next-cycle)
                    (take-while identity))]
    (+ (signal-strength-at 20 cycles)
       (signal-strength-at 60 cycles)
       (signal-strength-at 100 cycles)
       (signal-strength-at 140 cycles)
       (signal-strength-at 180 cycles)
       (signal-strength-at 220 cycles))))
