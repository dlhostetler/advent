(ns advent.2024.day12
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def init-plots
  (->> "resources/2024/day12.input"
       grid/slurp
       (map-vals str)))

(defn region-at [plots [p plant]]
  (->> (grid/flood plots {:fill-with "*"
                          :should-fill? (partial = plant)
                          :start-point p})
       :grid
       (filter (comp (partial = "*") val))
       (map-vals (constantly plant))))

(defn split-regions [plots]
  (loop [plots plots
         regions []]
    (if-let [plot (->> plots
                       (remove (comp (partial = ".") val))
                       first)]
      (let [region (region-at plots plot)]
        (recur (into plots
                     (for [p (keys region)]
                       [p "."]))
               (conj regions region)))
      regions)))

(defn area [region]
  (count region))

(defn outside-corner? [region dir0 dir1 p]
  (and (not (contains? region (dir0 p)))
       (not (contains? region (dir1 p)))))

(defn inside-corner? [region dir-empty dir-full0 dir-full1 p]
  (and (not (contains? region (dir-empty p)))
       (contains? region (dir-full0 p))
       (contains? region (dir-full1 p))))

(defn num-corners [region p]
  (->> [(outside-corner? region grid/north grid/west p)
        (outside-corner? region grid/north grid/east p)
        (outside-corner? region grid/south grid/east p)
        (outside-corner? region grid/south grid/west p)
        (inside-corner? region grid/northwest grid/north grid/west p)
        (inside-corner? region grid/northeast grid/north grid/east p)
        (inside-corner? region grid/southeast grid/south grid/east p)
        (inside-corner? region grid/southwest grid/south grid/west p)]
       (filter true?)
       count))

(defn num-sides [region]
  (->> region
       keys
       sort
       (map (partial num-corners region))
       (reduce +)))

(defn price [region]
  (* (area region) (num-sides region)))

(defn run []
  (->> init-plots
       split-regions
       (map price)
       (reduce +)))
