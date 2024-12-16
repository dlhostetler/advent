(ns advent.2024.day16
  (:require [advent.grid :as grid]
            [clojure.set :as set]
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

(defn visited-state [[path dir]]
  [(last path) dir])

(defn dijkstra
  [initial final? generate-moves]
  (let [to-visit (PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited {}]
      (if (or (nil? state) (final? state cost))
        cost
        (let [visited-cost (visited (visited-state state))]
          (if (and visited-cost (< visited-cost cost))
            (recur (.poll to-visit) visited)
            (do
              (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
                (.add to-visit [nxt-cost nxt-state]))
              (recur (.poll to-visit)
                     (assoc visited (visited-state state) cost)))))))))

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

(defn neighbors [[[path dir] cost]]
  (for [[x' y' dir' cost'] ((get next-positions dir) (last path))
        :when (grid/valid-point-or-nil tiles [x' y'])
        :when (not= (tiles [x' y']) "#")]
    [[(conj path [x' y']) dir'] (+ cost cost')]))

(defn run []
  (let [start [[start] :e]                                  ;; x y dir path
        best-points (atom nil)
        best-seats (atom #{})]
    (dijkstra start
              (fn [[path] points]
                (when (= end (last path))
                  (when (nil? @best-points)
                    (println "best points" points)
                    (reset! best-points points))
                  (if (= points @best-points)
                    (do
                      (println "adding points")
                      (swap! best-seats (fn [seats]
                                          (apply conj seats path)))
                      false)
                    true)))
              neighbors)
    (count @best-seats)))
