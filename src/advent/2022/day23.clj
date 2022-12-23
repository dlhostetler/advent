(ns advent.2022.day23
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [plumbing.core :refer :all]))

(def init-elves
  (->> "resources/2022/day23.input"
       io/reader
       grid/slurp
       (filter (comp (partial = \#) last))
       keys
       set))

(defn to-grid [elves]
  (->> (map vector elves (repeat :elf))
       (into {})))

(defn display [elves]
  (grid/print (to-grid elves)
              {:elf "#"
               :nothing "."}
              {:default :nothing
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(def round-directions
  (cycle [[grid/north grid/northeast grid/northwest]
          [grid/south grid/southeast grid/southwest]
          [grid/west grid/northwest grid/southwest]
          [grid/east grid/northeast grid/southeast]]))

(defn stay? [elves elf]
  (let [targets [(grid/north elf)
                 (grid/northeast elf)
                 (grid/east elf)
                 (grid/southeast elf)
                 (grid/south elf)
                 (grid/southwest elf)
                 (grid/west elf)
                 (grid/northwest elf)]]
    (empty? (set/intersection (set targets) elves))))

(defn propose-direction [elves directions elf]
  (when-not (empty? directions)
    (let [targets (->> directions
                      first
                      (map (fn [dir-fn] (dir-fn elf))))]
      (if (empty? (set/intersection (set targets) elves))
        {(first targets) #{elf}}
        (recur elves (rest directions) elf)))))

(defn propose-one [elves direction elf]
  (when-not (stay? elves elf)
    (propose-direction elves direction elf)))

(defn propose-all [directions elves]
  (->> elves
       (map (partial propose-one elves directions))
       (remove nil?)
       (apply merge-with set/union)))

(defn apply-move [elves [to from]]
  (-> elves (disj from) (conj to)))

(defn round [directions elves]
  (let [proposals (propose-all directions elves)
        moves (->> proposals
                   (remove (fn [[_ elves]] (> (count elves) 1)))
                   (map-vals first)
                   (into {}))]
    (reduce apply-move elves moves)))

(defn do-rounds
  ([elves] (do-rounds 0 elves))
  ([i elves]
   (let [directions (->> round-directions
                         (drop (mod i 4))
                         (take 4))
         moved-elves (round directions elves)]
     (if (not= elves moved-elves)
       (recur (inc i) moved-elves)
       (inc i)))))

(defn run []
  (->> init-elves
       do-rounds
       #_display))
