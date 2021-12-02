(ns advent.2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input []
  (-> "resources/2021/day2.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn ->command [commandStr]
  (let [[command n] (str/split commandStr #" ")]
    {:command (keyword command)
     :amount (Integer/parseInt n)}))

(defn is-command [targetCommand]
  (fn [{ command :command }]
    (= command targetCommand)))

(defn traveled [commands targetCommand]
  (->> commands
       (filter (is-command targetCommand))
       (map :amount)
       (reduce +)))

(defn run []
  (let [commands (map ->command (input))
        horizontal (traveled commands :forward)
        depth (- (traveled commands :down)
                 (traveled commands :up))]
    (* horizontal depth)))
