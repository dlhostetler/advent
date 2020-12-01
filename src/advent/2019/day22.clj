(ns advent.2019.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn new-stack [cards]
  (reverse cards))

(defn cut-cards [cards n]
  (cond
    (pos? n)
    (concat (drop n cards) (take n cards))
    (neg? n)
    (let [x (- (count cards) (Math/abs n))]
      (concat (drop x cards) (take x cards)))
    :else
    cards))

(defn increment-cards [cards n]
  (let [orig-cards (vec cards)
        indices (->> (repeat (range (count cards)))
                     flatten
                     (take-nth n)
                     (take (count cards)))]
    (loop [cards orig-cards
           indices indices
           i 0]
      (if-not (empty? indices)
        (recur (assoc cards (first indices) (nth orig-cards i))
               (rest indices)
               (inc i))
        cards))))

(defn s->n [s]
  (-> s
      (str/split #" ")
      last
      Integer/parseInt))

(defn resolve-shuffle [s]
  (cond
    (= "deal into new stack" s)
    new-stack
    (str/starts-with? s "cut")
    #(cut-cards % (s->n s))
    (str/starts-with? s "deal with increment")
    #(increment-cards % (s->n s))))

(defn run []
  (-> (loop [cards (range 10007)
             shuffles (->> (io/reader "resources/2019/day22.input")
                           line-seq
                           (map resolve-shuffle))]
        (if-not (empty? shuffles)
          (let [shuffle (first shuffles)]
            (recur (shuffle cards)
                   (rest shuffles)))
          cards))
      (.indexOf 2019)))