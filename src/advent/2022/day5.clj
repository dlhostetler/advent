(ns advent.2022.day5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (slurp (io/reader "resources/2022/day5.input")))

(defn parse-crate [c]
  (if (empty? c)
    nil
    (->> (seq c)
         (drop 1)
         first
         str
         keyword)))

(defn parse-stack-line [s]
  (->> (seq s)
       (map str)
       (partition-all 4)
       (map str/join)
       (map str/trim)
       (map parse-crate)))

(defn drop-nils [stack]
  (->> stack
       (filter identity)
       vec))

(defn transpose [lists]
  (apply map vector lists))

(defn parse-stacks [s]
  (let [lines (-> s
                  (str/split #"\n\n")
                  first
                  (str/split #"\n"))]
    (->> lines
         drop-last
         (map parse-stack-line)
         transpose
         (mapv drop-nils))))

(defn parse-instruction [s]
  (let [[amount from to] (->> (re-matches #"move (\d+) from (\d+) to (\d+)" s)
                              (drop 1)
                              (map #(Integer/parseInt %)))]
    {:amount amount
     :from (dec from)
     :to (dec to)}))

(defn parse-instructions [s]
  (map parse-instruction
       (-> s
           (str/split #"\n\n")
           last
           (str/split #"\n"))))

(defn apply-instruction [stacks {amount :amount from :from to :to}]
  (let [from-stack (nth stacks from)
        to-stack (nth stacks to)
        moved (->> from-stack
                   (take amount)
                   reverse
                   vec)]
    (-> stacks
        (assoc to (into moved to-stack))
        (assoc from (->> from-stack
                         (drop amount)
                         vec)))))

(defn run []
  (let [stacks (parse-stacks input)
        instructions (parse-instructions input)]
    (->> instructions
         (reduce apply-instruction stacks)
         (map first)
         (map name)
         (apply str))))
