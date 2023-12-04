(ns advent.2023.day4
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day4.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn parse-card [s]
  (let [[game all-nums] (str/split s #":")
        [winners nums] (str/split all-nums #"\|")]
    {:id (Integer/parseInt (re-find #"\d+" game))
     :nums (->> (str/split (str/trim nums) #" ")
                (remove empty?)
                (map str/trim)
                (map #(Integer/parseInt %))
                (into #{}))
     :winners (->> (str/split (str/trim winners) #" ")
                   (remove empty?)
                   (map str/trim)
                   (map #(Integer/parseInt %))
                   (into #{}))}))

(defn ->score [{nums :nums winners :winners}]
  (let [total-matches (count (set/intersection nums winners))]
    (if (not (zero? total-matches))
      (int (Math/pow 2 (dec total-matches)))
      0)))

(defn run []
  (->> input
       (map parse-card)
       (map ->score)
       (reduce +)))
