(ns advent.2022.day9
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-position [s]
  (let [[direction amount] (str/split s #" ")]
    (repeat (Integer/parseInt amount) (keyword direction))))

(def head-dirs
  (->> "resources/2022/day9.input"
       io/reader
       line-seq
       (mapcat parse-position)))

(def init-state
  {:head [0 0]
   :head-dirs head-dirs
   :tail [0 0]})

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

(defn adjust-tail [{head :head tail :tail :as state}]
  (let [offset (mapv - head tail)]
    (update state :tail #(mapv + (adjust-offset offset) %))))

(defn next-state* [state]
  (let [dir (-> state :head-dirs first)]
    (-> state
        (update-in [:head (dir->coord dir)] (dir->op dir) 1)
        (update :head-dirs rest)
        adjust-tail)))

(defn next-state [state]
  (when (not (empty? (:head-dirs state)))
    (next-state* state)))

(defn run []
  (->> init-state
       (seq/successive next-state)
       (take-while identity)
       (map :tail)
       set
       count))
