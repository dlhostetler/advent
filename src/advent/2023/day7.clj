(ns advent.2023.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day7.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn parse-hand [s]
  (let [[cards bid] (str/split s #" ")]
    [cards (Integer/parseInt bid)]))

(def type->ranking {:high-card 0
                    :one-pair 1
                    :two-pair 2
                    :three-of-a-kind 3
                    :full-house 4
                    :four-of-a-kind 5
                    :five-of-a-kind 6})

(def card->ranking {\J 1
                    \2 2
                    \3 3
                    \4 4
                    \5 5
                    \6 6
                    \7 7
                    \8 8
                    \9 9
                    \T 10
                    \Q 12
                    \K 13
                    \A 14})

(defn cards->type [cards]
  (let [chars (frequencies cards)
        char-counts (->> chars vals sort reverse)]
    (cond
      (= 1 (count char-counts)) :five-of-a-kind
      (= 4 (first char-counts)) :four-of-a-kind
      (and (= 3 (first char-counts))
           (= 2 (second char-counts))) :full-house
      (= 3 (first char-counts)) :three-of-a-kind
      (and (= 2 (first char-counts))
           (= 2 (second char-counts))) :two-pair
      (= 2 (first char-counts)) :one-pair
      :else :high-card)))

(defn cards->type-wild [cards]
  (let [replacements (for [card (keys card->ranking)
                            :when (not= card \J)]
                        (str/replace cards \J card))]
    (->> replacements
         (map cards->type)
         (sort-by type->ranking)
         reverse
         first)))

(defn compare-hand [[cards0] [cards1]]
  (let [type-result (compare (-> cards0 cards->type-wild type->ranking)
                             (-> cards1 cards->type-wild type->ranking))]
    (if (zero? type-result)
      (compare (mapv card->ranking cards0)
               (mapv card->ranking cards1))
      type-result)))

(defn ->score [i [_ bid]]
  (* (inc i) bid))

(defn run []
  (->> input
       (map parse-hand)
       (sort compare-hand)
       (map-indexed ->score)
       (reduce +)))
