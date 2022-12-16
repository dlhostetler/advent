(ns advent.2022.day16
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

(defn parse-line [s]
  (let [[_ from flow-rate to-str] (re-matches #"Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.+)" s)]
    {:flow-rate (Integer/parseInt flow-rate)
     :to (->> (str/split to-str #"\s*,\s*")
              (map keyword))
     :valve (keyword from)}))

(def parsed-input
  (->> "resources/2022/day16.input"
       io/reader
       line-seq
       (map parse-line)))

(def valves
  (->> parsed-input
       (map :valve)
       (into #{})))

(def valve->flow-rate
  (->> parsed-input
       (map (fn [{flow-rate :flow-rate valve :valve}] [valve flow-rate]))
       (into {})))

(def target-valves
  (->> valve->flow-rate
       (remove (comp zero? last))
       (map first)
       (into #{})))

(def traversal-graph
  (->> (for [{from-valve :valve to :to} parsed-input
             to-valve to]
         [from-valve to-valve])
       (into [])
       (apply graph/digraph)))

(defn minutes-to-valve [from to]
  (-> (graph.alg/bf-path traversal-graph from to)
      count
      dec))

(alter-var-root #'minutes-to-valve memoize)

(defn current-pressure [closed]
  (->> closed
       (set/difference valves)
       (map valve->flow-rate)
       (reduce +)))

(alter-var-root #'current-pressure memoize)

(defn max-pressure [at closed minutes total-pressure]
  (let [p (current-pressure closed)]
    (cond
      ;; none left
      (empty? closed)
      (+ total-pressure (* p (- 31 minutes)))

      ;; no more minutes
      (> minutes 30)
      total-pressure

      ;; at closed valve, open
      (closed at)
      (recur at (disj closed at) (inc minutes) (+ total-pressure p))

      ;; try to move to next valve
      :else
      (->> closed
           (map (fn [valve]
                  (let [move-cost (minutes-to-valve at valve)]
                    (if (< (+ minutes move-cost) 30)
                      (max-pressure valve
                                    closed
                                    (+ minutes move-cost)
                                    (+ total-pressure (* p move-cost)))
                      (+ total-pressure (* p (- 31 minutes)))))))
           (remove nil?)
           (reduce max 0)))))

(alter-var-root #'max-pressure memoize)

(defn run []
  (max-pressure :AA target-valves 1 0))
