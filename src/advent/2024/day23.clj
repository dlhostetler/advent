(ns advent.2024.day23
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
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

(defn maybe-chief-lan? [computers]
  (some #(str/starts-with? % "t") computers))

(defn run []
  (->> (for [node (graph/nodes full-network)
             [successor0 successor1] (-> (graph/successors full-network node)
                                         (combo/combinations 2))
             :when ((graph/successors full-network successor0) successor1)]
         #{node successor0 successor1})
       (into #{})
       (filter maybe-chief-lan?)
       count))
