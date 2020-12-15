(ns advent.2020.day15
  (:require [advent.seq :as seq]
            [clojure.string :as str]))

(defn input []
  (->> (str/split "12,20,0,6,1,17,7" #",")
       (mapv #(Integer/parseInt %))))

(defn next-state [{:keys [last num-spoken] :as state}]
  (let [last-index (-> state :indices (get last))
        age (if last-index (- num-spoken last-index) 0)]
    (-> state
        (update :indices assoc last num-spoken)
        (assoc :last age)
        (update :num-spoken inc))))

(defn init-state [numbers]
  {:indices (->> (for [[i n] (->> numbers
                                  butlast
                                  (map-indexed vector))]
                   [n i])
                 (into {}))
   :last (last numbers)
   :num-spoken (-> numbers count dec)})

(defn run []
  (let [start (input)
        target-spoken 30000000
        remaining (->> start count (- target-spoken) inc)]
    (->> start
         init-state
         (seq/successive next-state)
         (take remaining)
         last
         :last)))
