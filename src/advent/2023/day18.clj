(ns advent.2023.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def dir-num->kw {"0" :r
                  "1" :d
                  "2" :l
                  "3" :u})

(def dir->next-point {:u (fn [[x y] amt] [x (- y amt)])
                      :r (fn [[x y] amt] [(+ x amt) y])
                      :d (fn [[x y] amt] [x (+ y amt)])
                      :l (fn [[x y] amt] [(- x amt) y])})

(defn parse-instruction [line]
  (let [[_ amtHex dirNum] (re-matches #".+\(#(.+)(\d)\)" line)
        dir (dir-num->kw dirNum)]
    {:amt (Integer/parseInt amtHex 16)
     :dir dir
     :next-point (dir->next-point dir)}))

(def instructions
  (->> (-> "resources/2023/day18.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-instruction)))

(defn dig [points {:keys [amt next-point]}]
  (conj points (next-point (last points) amt)))

(defn shoelace-area [perimeter]
  (println perimeter)
  (/ (->> perimeter
          (partition 2 1)
          (map (fn [[[x0 y0] [x1 y1]]]
                 (- (* y0 x1) (* x0 y1))))
          (reduce +)
          Math/abs)
     2))

(defn run []
  (let [points (reduce dig [[0 0]] instructions)]
    (+ (shoelace-area (reverse points))
       (/ (->> instructions (map :amt) (reduce +)) 2)
       1)))
