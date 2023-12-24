(ns advent.2023.day24
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def target-min 200000000000000)
(def target-max 400000000000000)

(defn parse-line [line]
  (let [[p v] (str/split line #" *@ *")]
    [(mapv #(Long/parseLong %) (str/split p #" *, *"))
     (mapv #(Long/parseLong %) (str/split v #" *, *"))]))

(def hail
  (->> (-> "resources/2023/day24.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-line)))

(defn intersection [[[px0 py0] [vx0 vy0] :as h0] [[px1 py1] [vx1 vy1] :as h1]]
  (let [dx (- px1 px0)
        dy (- py1 py0)
        det (- (* vx1 vy0)
               (* vy1 vx0))
        u (when-not (zero? det) (/ (- (* dy vx1) (* dx vy1))
                                   det))
        v (when-not (zero? det) (/ (- (* dy vx0) (* dx vy0))
                                   det))]
    (if (and u v (pos? u) (pos? v))
      [(float (+ px0 (* vx0 u))) (float (+ py0 (* vy0 u)))]
      nil)))

(defn inside? [[x y]]
  (and (<= target-min x target-max)
       (<= target-min y target-max)))

(defn run []
  (->> (combo/combinations hail 2)
       (map #(apply intersection %))
       (remove nil?)
       (map inside?)
       (filter true?)
       count))
