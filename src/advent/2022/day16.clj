(ns advent.2022.day16
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as graph.alg]
            [plumbing.core :refer :all]))

(def target-minutes 26)

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

(defn open [closed]
  (set/difference target-valves closed))

(alter-var-root #'current-pressure memoize)

(defn build-valves->pressure
  ([at closed minutes total-pressure]
   (build-valves->pressure {} at closed minutes total-pressure))
  ([valves->pressure at closed minutes total-pressure]
   (let [p (current-pressure closed)]
     (cond
       ;; none left
       (empty? closed)
       (update valves->pressure
               (open closed)
               (fnil max 0)
               (+ total-pressure (* p (- (inc target-minutes) minutes))))

       ;; no more minutes
       (> minutes target-minutes)
       (update valves->pressure closed (fnil max 0) total-pressure)

       ;; at closed valve, open
       (closed at)
       (recur valves->pressure
              at
              (disj closed at)
              (inc minutes)
              (+ total-pressure p))

       ;; try to move to next valve
       :else
       (->> closed
            (map (fn [valve]
                   (let [move-cost (minutes-to-valve at valve)]
                     (if (< (+ minutes move-cost) target-minutes)
                       (build-valves->pressure valves->pressure
                                               valve
                                               closed
                                               (+ minutes move-cost)
                                               (+ total-pressure (* p move-cost)))
                       (update valves->pressure
                               (open closed)
                               (fnil max 0)
                               (+ total-pressure (* p (- (inc target-minutes) minutes))))))))
            (apply merge-with max))))))

(alter-var-root #'build-valves->pressure memoize)

(defn disjoint? [[valves0 valves1]]
  (empty? (set/intersection valves0 valves1)))

(defonce valves->pressure (build-valves->pressure :AA target-valves 1 0))

(defn combined-pressure [valves]
  (->> valves
       (map valves->pressure)
       (reduce +)))

(defn run []
  (->> (-> valves->pressure
           keys
           (combo/combinations 2))
       (filter disjoint?)
       (map combined-pressure)
       (reduce max)))
