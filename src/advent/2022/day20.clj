(ns advent.2022.day20
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all])
  (:import (java.util LinkedList)))

(def decrypt-key 811589153)

(def numbers
  (->> "resources/2022/day20.input"
       io/reader
       line-seq
       (map #(Integer/parseInt %))
       (map-indexed (fn [i n] {:i i :n (* n decrypt-key)}))))

(defn into-list [^LinkedList numbers n]
  (.add numbers n)
  numbers)

(defn list-of [numbers]
  (reduce into-list (LinkedList.) numbers))

(defn mix [^LinkedList numbers number]
  (let [from (.indexOf numbers number)
        _ (.remove numbers from)
        to (mod (+ from (:n number)) (count numbers))]
    #_(println "moving" number "from" from "to" to)
    (if (zero? to)
      (.addLast numbers number)
      (.add numbers to number))
    #_(println (seq numbers))
    numbers))

(defn mix-times [mixed times]
  (if (zero? times)
    mixed
    (recur (reduce mix mixed numbers)
           (dec times))))

(defn coordinates [^LinkedList numbers]
  (let [zero-index (->> numbers
                        (map-indexed vector)
                        (filter (comp zero? :n last))
                        first
                        first)]
    (->> (for [offset [1000 2000 3000]]
           (.get numbers (mod (+ zero-index offset) (count numbers))))
         (map :n)
         (into []))))

(defn run []
  (->> (mix-times (list-of numbers) 10)
       coordinates
       (reduce +)))
