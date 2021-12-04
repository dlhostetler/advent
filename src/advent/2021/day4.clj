(ns advent.2021.day4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn board2matrix [board]
  (->> (str/split board #"\n")
       (map #(str/trim %))
       (map #(str/split % #"\s+"))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn input []
  (let [[nums & boards] (-> "resources/2021/day4.input"
                            io/reader
                            slurp
                            (str/split #"\n\n"))]
    {:nums (map #(Integer/parseInt %) (str/split nums #","))
     :boards (map board2matrix boards)}))

(defn transpose [m]
  (apply mapv vector m))

(defn board2target [board]
  (concat (for [target board]
            {:board board
             :target target})
          (for [target (transpose board)]
            {:board board
             :target target})))

(defn ->score [all-picks target]
  (let [picks (->> target :target (map all-picks))]
    (-> target
        (assoc :picks picks)
        (assoc :maxpick (apply max picks)))))

(defn target->score [nums {board :board maxpick :maxpick picks :picks target :target}]
  (let [max-pick-idx (.indexOf picks maxpick)
        last-pick (nth target max-pick-idx)

        picked (-> (take-while #(not= last-pick %) nums)
                   vec
                   (conj last-pick))
        unmarked-nums (set/difference (set (flatten board))
                                      (set picked))]
    (->> unmarked-nums
         (reduce +)
         (* last-pick))))

(defn run []
  (let [{boards :boards nums :nums} (input)
        picks (->> nums
                   (map-indexed (fn [idx itm] [idx itm]))
                   (set/map-invert))
        targets (->> boards
                     (mapcat board2target)
                     (map (partial ->score picks)))
        lowest-max-pick (->> targets
                             (map :maxpick)
                             (apply min))
        target (->> targets
                    (filter #(= (:maxpick %) lowest-max-pick))
                    first)]
    (target->score nums target)))
