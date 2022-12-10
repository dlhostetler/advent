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

(defn draw-into [buffer {cycle-num :cycle-num x :x}]
  (let [ray-x (dec cycle-num)
        row-x (mod ray-x 40)]
    (if (or (= row-x (dec x))
            (= row-x x)
            (= row-x (inc x)))
      (assoc buffer ray-x "#")
      buffer)))

(defn run []
  (let [cycles (->> init-state
                    (seq/successive next-cycle)
                    (take-while identity))
        buffer (->> (repeat 240 " ")
                    (into []))]
    (->> cycles
         (reduce draw-into buffer)
         (partition-all 40)
         (map #(apply str %)))))
