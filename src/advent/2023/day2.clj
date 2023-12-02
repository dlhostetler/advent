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

(defn ->fewest-cubes [{rounds :rounds :as game}]
  (assoc game :fewest (apply merge-with max rounds)))

(defn power [{fewest :fewest}]
  (let [{red :red green :green blue :blue} fewest]
    (* red green blue)))

(defn run []
  (->> input
       (map parse-game)
       (map ->fewest-cubes)
       (map power)
       (reduce +)))
