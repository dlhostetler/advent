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

(defn count-stones [blinks stone]
  (cond
    (zero? blinks)
    1

    (zero? stone)
    (count-stones (dec blinks) 1)

    (even-digits? stone)
    (let [[stone0 stone1] (split-stone stone)]
      (+ (count-stones (dec blinks) stone0)
         (count-stones (dec blinks) stone1)))

    :else
    (count-stones (dec blinks) (* stone 2024))))

(alter-var-root #'count-stones memoize)

(defn run []
  (->> input
       (map (partial count-stones 75))
       (reduce +)))
