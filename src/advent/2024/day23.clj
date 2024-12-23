(ns advent.2024.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]))

(defn into-sets [sets [from to]]
  (update sets from (fnil conj #{}) to))

(def edges
  (->> (-> "resources/2024/day23.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map #(str/split % #"-"))
       (reduce into-sets {})))

(def full-network
  (graph/graph edges))

(defn run []
  (->> (graph.alg/maximal-cliques full-network)
       (sort-by count)
       last
       sort
       (str/join ",")))
