(ns advent.2024.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-machine [line]
  (let [[a b prize] (str/split line #"\n")
        [_ ax ay] (re-matches #"Button A: X[+](\d+), Y[+](\d+)" a)
        [_ bx by] (re-matches #"Button B: X[+](\d+), Y[+](\d+)" b)
        [_ prizex prizey] (re-matches #"Prize: X=(\d+), Y=(\d+)" prize)]
    {:a [(Integer/parseInt ax) (Integer/parseInt ay)]
     :b [(Integer/parseInt bx) (Integer/parseInt by)]
     :prize [(+ 10000000000000 (Integer/parseInt prizex))
             (+ 10000000000000 (Integer/parseInt prizey))]}))

(def machines
  (map parse-machine (-> "resources/2024/day13.input"
                         io/reader
                         slurp
                         (str/split #"\n\n"))))

(defn determinant [[x0 y0] [x1 y1]]
  (- (* x0 y1) (* y0 x1)))

(defn min-presses [{:keys [a b prize]}]
  (let [det-presses (determinant a b)
        d1 (determinant a prize)
        d2 (determinant prize b)]
    (when (and (zero? (mod d2 det-presses))
               (zero? (mod d1 det-presses)))
      [(long (/ d2 det-presses)) (long (/ d1 det-presses))])))

(defn cost [[presses-a presses-b]]
  (long (+ (* 3 presses-a) presses-b)))

(defn run []
  (->> machines
       (map min-presses)
       (remove nil?)
       (map cost)
       (reduce +)))
