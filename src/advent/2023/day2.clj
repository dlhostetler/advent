(ns advent.2023.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> "resources/2023/day2.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn parse-round [round]
  (into {}
        (for [cubes (map str/trim (str/split round #","))
              :let [[n color] (str/split cubes #" ")]]
          [(keyword color) (Integer/parseInt n)])))

(defn parse-game [s]
  (let [[game rounds] (str/split s #":")]
    {:id (Integer/parseInt (re-find #"\d+" game))
     :rounds (->> (str/split rounds #";")
                  (map str/trim)
                  (mapv parse-round))}))

(defn possible-round? [{red :red green :green blue :blue}]
  (and (if red (<= red 12) true)
       (if green (<= green 13) true)
       (if blue (<= blue 14) true)))

(defn possible-game? [{rounds :rounds}]
  (every? possible-round? rounds))

(defn run []
  (->> input
       (map parse-game)
       (filter possible-game?)
       (map :id)
       (reduce +)))
