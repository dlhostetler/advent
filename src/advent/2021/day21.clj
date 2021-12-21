(ns advent.2021.day21
  (:require [plumbing.core :refer :all]))

(def player-1-start 10)
(def player-2-start 7)

(def roll->times
  {3 1
   4 3
   5 6
   6 7
   7 6
   8 3
   9 1})

(declare play)

(defn clamp-space [space]
  (if (> space 10)
    (recur (- space 10))
    space))

(defn update-score [{:keys [space] :as player-state}]
  (update player-state :score + space))

(defn player-turn [player roll]
  (-> player
      (update :space + roll)
      (update :space clamp-space)
      update-score))

(defn split-universe
  [player-1 player-2 previous-wins [roll times]]
  (->> (player-turn player-1 roll)
       (play player-2)
       reverse
       (map * (repeat times))
       (mapv + previous-wins)))

(defn play [player-1 player-2]
  (if (< (:score player-2) 21)
    (reduce (partial split-universe player-1 player-2) [0 0] roll->times)
    [0 1]))

(alter-var-root #'play memoize)

(defn run []
  (->> (play {:score 0 :space player-1-start}
             {:score 0 :space player-2-start})
       (reduce max)))
