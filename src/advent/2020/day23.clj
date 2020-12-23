(ns advent.2020.day23
  (:require [advent.seq :as seq]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def cups (->> "523764819"
               seq
               (map str)
               (mapv #(Integer/parseInt %))))

(defn grab-cups [cups]
  [(into [(first cups)] (drop 4 cups))
   (->> cups (drop 1) (take 3))])

(defn next-destination [cups i]
  (let [next-i (dec i)]
    (if (zero? next-i) (count cups) next-i)))

(defn find-destination [cups]
  (let [[current x y z] cups
        invalid #{x y z}]
    (loop [destination (next-destination cups current)]
      (if (contains? invalid destination)
        (recur (next-destination cups destination))
        destination))))

(defn drop-cups [remaining-cups moved destination]
  (let [i (.indexOf remaining-cups destination)
        [head tail] (split-at (inc i) remaining-cups)]
    (concat head moved tail)))

(defn rotate [cups]
  (conj (vec (rest cups)) (first cups)))

(defn next-move [cups]
  (let [destination (find-destination cups)
        [remaining-cups moved] (grab-cups cups)]
    (-> (drop-cups remaining-cups moved destination)
        rotate)))

(defn answer [cups]
  (let [[head tail] (split-at (.indexOf cups 1) cups)]
    (->> (concat (rest tail) head)
         (map str)
         (str/join))))

(defn run []
  (->> cups
       (seq/successive next-move)
       (take 101)
       last
       answer))
