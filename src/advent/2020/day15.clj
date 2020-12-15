(ns advent.2020.day15
  (:require [advent.seq :as seq]
            [clojure.string :as str]))

(defn input []
  (->> (str/split "12,20,0,6,1,17,7" #",")
       (mapv #(Integer/parseInt %))))

(defn next-state [state]
  (let [n (-> state :numbers last)
        n-idx (-> state :indices (get n))
        current-idx (-> state :numbers count dec)
        age (if n-idx (- current-idx n-idx) 0)]
    (-> state
        (update :numbers conj age)
        (update :indices assoc n current-idx))))

(defn init-state [numbers]
  {:numbers numbers
   :indices (->> (for [[i n] (->> numbers
                                  butlast
                                  (map-indexed vector))]
                   [n i])
                 (into {}))})
(defn run []
  (let [start (input)]
    (->> start
         init-state
         (seq/successive next-state)
         (take (inc (- 2020 (count start))))
         last
         :numbers
         last)))
