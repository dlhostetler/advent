(ns advent.2020.day5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def last-row 127)
(def last-col 7)
(def row-halves {\B :upper
                 \F :lower})
(def col-halves {\R :upper
                 \L :lower})

(defn input []
  (-> "resources/2020/day5.input"
      io/reader
      line-seq))

(defn seat->row-halves [seat]
  (->> (subs seat 0 7)
       (map row-halves)))

(defn seat->col-halves [seat]
  (->> (subs seat 7)
       (map col-halves)))

(defn search [all-halves last-x]
  (loop [halves all-halves
         lower 0
         upper last-x]
    (let [half (first halves)
          mid (+ lower (/ (- upper lower) 2))]
      (if (= 1 (count halves))
        ;; should be only two choices
        (if (= half :lower) lower upper)
        ;; keep going
        (if (= half :lower)
          (recur (rest halves) lower (-> mid (Math/floor) int))
          (recur (rest halves) (-> mid (Math/ceil) int) upper))))))

(defn seat->id [seat]
  (let [row (search (seat->row-halves seat)
                    last-row)
        col (search (seat->col-halves seat)
                    last-col)]
    (+ (* row 8) col)))

(defn run []
  (let [occupied-ids (->> (input)
                          (map seat->id)
                          (set))
        all-ids (set (range (apply min occupied-ids)
                            (apply max occupied-ids)))]
    (first (set/difference all-ids occupied-ids))))
