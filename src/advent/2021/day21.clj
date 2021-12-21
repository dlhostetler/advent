(ns advent.2021.day21
  (:require [advent.seq :as seq]
            [plumbing.core :refer :all]))

(def player-1-start 10)
(def player-2-start 7)

(def die-vals (range 100))

(defn player-won? [state player]
  (>= (get-in state [player :score]) 1000))

(defn winner [state]
  (cond
    (player-won? state :player-1)
    :player-1
    (player-won? state :player-2)
    :player-2
    :else
    nil))

(defn loser [state]
  (when-let [player (winner state)]
    (if (= player :player-1) :player-2 :player-1)))

(defn continue? [state]
  (not (winner state)))

(defn next-roll [{:keys [rolls] :as state}]
  [(inc (nth die-vals (mod rolls 100)))
   (update state :rolls inc)])

(defn clamp-space [space]
  (if (> space 10)
    (recur (- space 10))
    space))

(defn update-score [{:keys [space] :as player-state}]
  (update player-state :score + space))

(defn player-turn [state player]
  (if (continue? state)
    (let [[r1 state] (next-roll state)
          [r2 state] (next-roll state)
          [r3 state] (next-roll state)]
      (-> state
          (update-in [player :space] + r1 r2 r3)
          (update-in [player :space] clamp-space)
          (update player update-score)))
    state))

(defn turn [state]
  (-> state
      (player-turn :player-1)
      (player-turn :player-2)))

(defn answer [state]
  (* (get-in state [(loser state) :score])
     (:rolls state)))

(defn run []
  (->> {:player-1 {:score 0
                   :space player-1-start}
        :player-2 {:score 0
                   :space player-2-start}
        :rolls 0}
       (seq/successive turn)
       (drop-while continue?)
       first
       answer))
