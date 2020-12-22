(ns advent.2020.day22
  (:require [plumbing.core :refer :all]))

(def player1 [10
              39
              16
              32
              5
              46
              47
              45
              48
              26
              36
              27
              24
              37
              49
              25
              30
              13
              23
              1
              9
              3
              31
              14
              4
              ])

(def player2 [2
              15
              29
              41
              11
              21
              8
              44
              38
              19
              12
              20
              40
              17
              22
              35
              34
              42
              50
              6
              33
              7
              18
              28
              43])

(defn cards->winner [card1 card2]
  (if (> card1 card2) :player1 :player2))

(defn state->winner [{:keys [player2]}]
  (if (empty? player2) :player1 :player2))

(defn score [{:keys [player1 player2]}]
  (let [cards (if (empty? player1) player2 player1)]
    (->> (map * cards (-> cards count inc range rest reverse))
         (reduce +))))

(defn already-seen? [{:keys [player1 player2 rounds]}]
  (contains? rounds [player1 player2]))

(defn done? [{:keys [player1 player2]}]
  (or (empty? player1)
      (empty? player2)))

(defn finish-round [next-state card1 card2 winner]
  (if (= winner :player1)
    (update next-state :player1 concat [card1 card2])
    (update next-state :player2 concat [card2 card1])))

(defn round [{:keys [player1 player2] :as state}]
  (let [[card1 & next-player1] player1
        [card2 & next-player2] player2
        next-state (-> state
                       (assoc :player1 next-player1)
                       (assoc :player2 next-player2)
                       (update :rounds (fnil conj #{}) [player1 player2]))]
    (cond
      (already-seen? state)
      (assoc next-state :player2 [])

      (done? state)
      state

      (or (< (count next-player1) card1)
          (< (count next-player2) card2))
      (-> next-state
          (finish-round card1 card2 (cards->winner card1 card2))
          recur)

      :else
      (let [winner (-> {:player1 (take card1 next-player1)
                        :player2 (take card2 next-player2)}
                       round
                       state->winner)]
        (-> next-state
            (finish-round card1 card2 winner)
            recur)))))

(defn run []
  (score (round {:player1 player1
                 :player2 player2})))
