(ns advent.2024.day16
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all])
  (:import (java.util PriorityQueue)))

(def tiles
  (->> "resources/2024/day16.input"
       grid/slurp
       (map-vals str)))

(def start
  (->> tiles
       (filter (comp (partial = "S") val))
       first
       first))

(def end
  (->> tiles
       (filter (comp (partial = "E") val))
       first
       first))

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

(def next-positions
  {:n (fn [point]
        [(conj (grid/north point) :n 1)
         (conj (grid/east point) :e 1001)
         (conj (grid/west point) :w 1001)])
   :s (fn [point]
        [(conj (grid/south point) :s 1)
         (conj (grid/east point) :e 1001)
         (conj (grid/west point) :w 1001)])
   :e (fn [point]
        [(conj (grid/south point) :s 1001)
         (conj (grid/north point) :n 1001)
         (conj (grid/east point) :e 1)])
   :w (fn [point]
        [(conj (grid/south point) :s 1001)
         (conj (grid/north point) :n 1001)
         (conj (grid/west point) :w 1)])})

(defn neighbors [[[x y dir] cost]]
  (for [[x' y' dir' cost'] ((get next-positions dir) [x y])
        :when (grid/valid-point-or-nil tiles [x' y'])
        :when (not= (tiles [x' y']) "#")]
    [[x' y' dir'] (+ cost cost')]))

(defn run []
  (let [start [(first start) (last start) :e]]              ;; x y dir
    (dijkstra start
              (fn [[x y _]]
                (= end [x y]))
              neighbors)))
