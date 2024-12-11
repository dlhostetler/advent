(ns advent.2024.day11
  (:require [advent.seq :as seq]))

(def input
  [112 1110 163902 0 7656027 83039 9 74])

(defn even-digits? [n]
  (-> n str seq count even?))

(defn split-stone [n]
  (let [nstr (-> n str seq vec)
        mid (int (/ (count nstr) 2))]
    [(Integer/parseInt (apply str (subvec nstr 0 mid)))
     (Integer/parseInt (apply str (subvec nstr mid)))]))

(defn stone->stones [stone]
  (cond
    (zero? stone)
    [1]

    (even-digits? stone)
    (split-stone stone)

    :else
    [(* stone 2024)]))

(defn next-stones [stones]
  (->> stones (map stone->stones) flatten))

(defn run []
  (->> input
       (seq/successive next-stones)
       (drop 25)
       first
       count))
