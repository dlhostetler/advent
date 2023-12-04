(ns advent.2023.day4
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-card [s]
  (let [[game all-nums] (str/split s #":")
        [winners nums] (str/split all-nums #"\|")]
    [(Integer/parseInt (re-find #"\d+" game))
     {:nums (->> (str/split (str/trim nums) #" ")
                 (remove empty?)
                 (map str/trim)
                 (map #(Integer/parseInt %))
                 (into #{}))
      :winners (->> (str/split (str/trim winners) #" ")
                    (remove empty?)
                    (map str/trim)
                    (map #(Integer/parseInt %))
                    (into #{}))}]))

(def cards
  (->> (-> "resources/2023/day4.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-card)))

(defn card->cards [cardsplosion [card-num {nums :nums winners :winners}]]
  (let [total-matches (count (set/intersection nums winners))
        new-cards (into {}
                        (for [i (range total-matches)]
                          [(+ (inc card-num) i)
                           (get cardsplosion card-num)]))]
    (merge-with + cardsplosion new-cards)))

(defn run []
  (->> (reduce card->cards (zipmap (map first cards) (repeat 1)) cards)
       vals
       (reduce +)))
