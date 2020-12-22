(ns advent.2020.day22
  (:require [advent.seq :as seq]
            [plumbing.core :refer :all]))

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
              4])

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

(defn next-state [{:keys [player1 player2] :as state}]
  (let [card1 (first player1)
        card2 (first player2)]
    (if (and card1 card2)
      (if (> card1 card2)
        {:player1 (into (-> player1 rest vec)
                        [card1 card2])
         :player2 (-> player2 rest vec)}
        {:player1 (-> player1 rest vec)
         :player2 (into (-> player2 rest vec)
                        [card2 card1])})
      state)))

(defn done? [{:keys [player1 player2]}]
  (or (empty? player1)
      (empty? player2)))

(defn score [{:keys [player1 player2]}]
  (let [cards (if (empty? player1) player2 player1)]
    (->> (map * cards (-> cards count inc range rest reverse))
         (reduce +))))

(defn run []
  (->> (seq/successive next-state {:player1 player1
                                   :player2 player2})
       (filter done?)
       first
       score))
