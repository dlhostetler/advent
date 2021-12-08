(ns advent.2021.day8
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn display->segments [display]
  (->> display (map str) (map keyword) (into #{})))

(defn parse-entry [s]
  (let [[signals outputs] (str/split s #" \| ")]
    {:outputs (->> (str/split outputs #" ")
                   (map display->segments))
     :signals (->> (str/split signals #" ")
                   (map display->segments))}))

(defn entries []
  (->> (-> "resources/2021/day8.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-entry)))

(defn signals-by-length [signals len]
  (->> signals (filter #(= (count %) len)) set))

(defn one-signal-by-length [signals len]
  (first (signals-by-length signals len)))

(defn overlay [possibilities super-segments]
  (->> possibilities
       (filter #(set/subset? % super-segments))
       first))

(defn overlaid [possibilities sub-segments]
  (->> possibilities
       (filter (partial set/subset? sub-segments))
       first))

(defn decode-segments
  [patterns]
  (let [five-segmented (signals-by-length patterns 5)
        six-segmented (signals-by-length patterns 6)

        one (one-signal-by-length patterns 2)
        seven (one-signal-by-length patterns 3)
        four (one-signal-by-length patterns 4)
        eight (one-signal-by-length patterns 7)
        ;; 3 is overlaid on top of 1 and no other possibilities
        three (overlaid five-segmented one)
        ;; 9 is overlaid on top of 4 and no other possibilities
        nine (overlaid six-segmented four)
        ;; if 9 is known, 0 is overlaid on top of 1
        zero (overlaid (disj six-segmented nine) one)
        ;; if 9 and 0 are known, 6 is the leftover
        six (-> six-segmented (disj zero nine) first)
        ;; 6 is overlaid on top of 5
        five (overlay five-segmented six)
        ;; if 3 and 5 are known, 2 is the leftover
        two (-> five-segmented (disj three five) first)]
    {zero "0"
     one "1"
     two "2"
     three "3"
     four "4"
     five "5"
     six "6"
     seven "7"
     eight "8"
     nine "9"}))

(defn entry->int [{outputs :outputs signals :signals}]
  (let [segments->num (decode-segments signals)]
    (->> outputs
         (map segments->num)
         str/join
         (Integer/parseInt))))

(defn run []
  (->> (entries)
       (take 1)
       (map entry->int)
       (reduce +)))