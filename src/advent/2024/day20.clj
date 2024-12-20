(ns advent.2024.day20
  (:require [advent.grid :as grid]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]
            [plumbing.core :refer :all]))

(def racetrack
  (->> "resources/2024/day20.input"
       grid/slurp
       (map-vals str)))

(def start
  (->> racetrack
       (filter (comp (partial = "S") val))
       first
       first))

(def end
  (->> racetrack
       (filter (comp (partial = "E") val))
       first
       first))

(defn initial-edges []
  (into {}
        (for [[p tile] racetrack
              :when (not= tile "#")]
          [p (into []
                   (for [neighbor (grid/cardinal-neighbors racetrack p)
                         :when (not= (get racetrack neighbor) "#")]
                     neighbor))])))

(defn track? [p]
  (when-let [p (grid/valid-point-or-nil racetrack p)]
    (not= "#" (racetrack p))))

(defn cheat-passthrough [track0 wall track1]
  (when (and (track? track0) (track? track1))
    [[track0 wall] [wall track1]]))

(defn cheat-edges [wall]
  (->> [(cheat-passthrough (grid/west wall) wall (grid/east wall))
        (cheat-passthrough (grid/east wall) wall (grid/west wall))
        (cheat-passthrough (grid/north wall) wall (grid/south wall))
        (cheat-passthrough (grid/south wall) wall (grid/north wall))]
       (remove nil?)))

(defn potential-cheats []
  (->> racetrack
       (filter (comp (partial = "#") val))
       (map first)
       (map cheat-edges)
       (remove empty?)
       (apply concat)))

(defn cheat [graph [from-edge to-edge :as passthrough]]
  (graph/add-edges graph from-edge to-edge))

(defn race-time [graph]
  (dec (count (graph.alg/bf-path graph start end))))

(defn cheating-race-time [graph passthrough]
  (let [t (race-time (cheat graph passthrough))]
    {:cheat passthrough
     :time t}))

(defn saved-time [normal-time {:keys [time]}]
  (- normal-time time))

(defn run []
  (let [graph (graph/digraph (initial-edges))
        normal-time (race-time graph)]
    (->> (potential-cheats)
         (pmap #(cheating-race-time graph %))
         (map #(saved-time normal-time %))
         (filter #(>= % 100))
         count)))
