(ns advent.2021.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-entry [s]
  (let [[signals outputs] (str/split s #" \| ")]
    {:outputs (str/split outputs #" ")
     :signals (str/split signals #" ")}))

(defn input []
  (->> (-> "resources/2021/day8.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-entry)))


(defn display->num [display]
  (let [num-segments (count display)]
    (cond
      (= num-segments 2)
      1
      (= num-segments 3)
      7
      (= num-segments 4)
      4
      (= num-segments 7)
      8
      :else
      display)))

(defn run []
  (->> (input)
       (mapcat :outputs)
       (map display->num)
       (filter int?)
       count))
