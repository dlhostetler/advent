(ns advent.2022.day25
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [plumbing.core :refer :all]))

(def c->digit {\= -2
               \- -1
               \0 0
               \1 1
               \2 2})

(def num->digit (set/map-invert c->digit))

(defn next-snafu-place [place]
  (if (nil? place)
    1
    (* place 5)))

(def snafu-places
  (let [place (atom nil)]
    (repeatedly #(swap! place next-snafu-place))))

(def input
  (->> "resources/2022/day25.input"
       io/reader
       line-seq))

(defn snafu-digit->int [i c]
  (* (c->digit c) (->> snafu-places (drop i) first)))

(defn snafu->int [snafu]
  (->> snafu
       reverse
       (map-indexed snafu-digit->int)
       (reduce +)))

(defn snafu-mod [i]
  (let [r (mod i 5)]
    (cond
      (= r 3) -2
      (= r 4) -1
      :else r)))

(defn int->snafu
  ([i] (int->snafu i ""))
  ([i snafu]
   (if (< i 3)
     (str (num->digit i) snafu)
     (let [d (snafu-mod i)]
       (recur (long (/ (- i d) 5))
              (str (num->digit d) snafu))))))

(defn run []
  (->> input
       (map snafu->int)
       (reduce +)
       (int->snafu)))
