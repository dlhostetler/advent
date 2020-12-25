(ns advent.2020.day25)

(def card-public-key 17607508)
(def door-public-key 15065270)

(defn loop-size-of [target-k]
  (loop [n 0
         k 1]
    (if (= k target-k)
      n
      (recur (inc n) (-> k (* 7) (mod 20201227))))))

(defn encryption-key [subject-key loop-size]
  (reduce #(mod (* %1 %2) 20201227)
          1
          (repeat loop-size subject-key)))

(defn run []
  (encryption-key card-public-key
                  (loop-size-of door-public-key)))
