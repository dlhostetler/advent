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
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def rps->score
  {:rock 1
   :paper 2
   :scissors 3})

(def round->score
  {[:rock :rock] 3
   [:rock :paper] 6
   [:rock :scissors] 0
   [:paper :rock] 0
   [:paper :paper] 3
   [:paper :scissors] 6
   [:scissors :rock] 6
   [:scissors :paper] 0
   [:scissors :scissors] 3})

(defn parse-round [s]
  (let [[opp me :as play] (->> (str/split s #" ")
                               (map c->rps))]
    {:me me
     :opponent opp
     :score (round->score play)}))

(defn calc-score [round]
  (+ (-> round :me rps->score) (:score round)))

(defn run []
  (->> input
       (map parse-round)
       (map calc-score)
       (reduce +)))
