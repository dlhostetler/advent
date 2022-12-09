(ns advent.2022.day9
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-movement [s]
  (let [[direction amount] (str/split s #" ")]
    (repeat (Integer/parseInt amount) (keyword direction))))

(def head-dirs
  (->> "resources/2022/day9.input"
       io/reader
       line-seq
       (mapcat parse-movement)))

(def init-state
  {:head [0 0]
   :dirs head-dirs
   :tails (repeat 9 [0 0])})

(def dir->coord
  {:L 0
   :U 1
   :R 0
   :D 1})

(def dir->op
  {:L -
   :U +
   :R +
   :D -})

(defn offset-too-far? [[offset-x offset-y]]
  (or (> (Math/abs ^int offset-x) 1)
      (> (Math/abs ^int offset-y) 1)))

(defn adjust-offset-value [v]
  (cond
    (> v 1)
    1
    (< v -1)
    -1
    :else
    v))

(defn adjust-offset [offset]
  (if (offset-too-far? offset)
    (mapv adjust-offset-value offset)
    [0 0]))

(defn adjust-knot [knot prev-knot]
  (let [offset (mapv - prev-knot knot)]
    (mapv + knot (adjust-offset offset))))

(defn adjust-tails [tails head]
  (loop [prev-knot head
         tails-to-adjust tails
         adjusted-tails []]
    (if (empty? tails-to-adjust)
      adjusted-tails
      (let [knot (-> tails-to-adjust
                     first
                     (adjust-knot prev-knot))]
        (recur knot
               (rest tails-to-adjust)
               (conj adjusted-tails knot))))))

(defn next-state* [{head :head :as state}]
  (let [dir (-> state :dirs first)
        next-head (update head (dir->coord dir) (dir->op dir) 1)]
    (-> state
        (assoc :head next-head)
        (update :tails adjust-tails next-head)
        (update :dirs rest))))

(defn next-state [state]
  (when (not (empty? (:dirs state)))
    (next-state* state)))

(defn run []
  (->> init-state
       (seq/successive next-state)
       (take-while identity)
       (map :tails)
       (map last)
       set
       count))
