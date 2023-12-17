(ns advent.2023.day17
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [plumbing.core :refer :all])
  (:import (java.util PriorityQueue)))

(def ^:private inf (Long/MAX_VALUE))

(def numbers
  (->> (-> "resources/2023/day17.input"
           io/reader
           grid/slurp)
       (map-vals str)
       (map-vals #(Integer/parseInt %))))

(def next-positions
  {:n (fn [point]
        [(conj (grid/north point) :n)
         (conj (grid/east point) :e)
         (conj (grid/west point) :w)])
   :s (fn [point]
        [(conj (grid/south point) :s)
         (conj (grid/east point) :e)
         (conj (grid/west point) :w)])
   :e (fn [point]
        [(conj (grid/south point) :s)
         (conj (grid/north point) :n)
         (conj (grid/east point) :e)])
   :w (fn [point]
        [(conj (grid/south point) :s)
         (conj (grid/north point) :n)
         (conj (grid/west point) :w)])})


(defn dijkstra
  [initial final? generate-moves]
  (let [to-visit (PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn neighbors [[[x y dir steps] cost]]
  (for [[x' y' dir'] ((get next-positions dir) [x y])
        :when (or (<= steps 2) (not= dir dir'))
        :when (grid/valid-point-or-nil numbers [x' y'])]
    [[x' y' dir' (if (= dir dir') (inc steps) 1)]
     (+ cost (get numbers [x' y']))]))

(defn run []
  (let [start [0 0 :e 0] ;; x y dir steps
        end [(grid/max-x numbers) (grid/max-y numbers)]]
    (dijkstra start
              #(every? true? (map = % end))
              neighbors)))
