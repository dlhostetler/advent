(ns advent.2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2022/day2.input"
      io/reader
      line-seq))

(def c->rps
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def c->outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(def outcome->score
  {:lose 0
   :draw 3
   :win 6})

(def rps->score
  {:rock 1
   :paper 2
   :scissors 3})

(def round->me
  {[:rock :lose] :scissors
   [:rock :draw] :rock
   [:rock :win] :paper
   [:paper :lose] :rock
   [:paper :draw] :paper
   [:paper :win] :scissors
   [:scissors :lose] :paper
   [:scissors :draw] :scissors
   [:scissors :win] :rock})

(defn parse-round [s]
  (let [[col0 col1] (str/split s #" ")
        opp (c->rps col0)
        outcome (c->outcome col1)]
    {:me (round->me [opp outcome])
     :opponent opp
     :score (outcome->score outcome)}))

(defn calc-score [round]
  (+ (-> round :me rps->score) (:score round)))

(defn run []
  (->> input
       (map parse-round)
       (map calc-score)
       (reduce +)))
