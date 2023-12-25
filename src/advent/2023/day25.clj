(ns advent.2023.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [loom.alg :as graph.alg]
            [loom.graph :as graph]
            [loom.io :as graph.io]
            [plumbing.core :refer :all]))

(defn ->edges [line]
  (let [[from tos] (str/split line #": ")]
    (for [to (str/split tos #" ")]
      [from to])))

(def edges
  (->> (-> "resources/2023/day25.input"
           io/reader
           slurp
           (str/split #"\n"))
       (mapcat ->edges)
       (into [])))

(def fullg (apply graph/graph edges))

(defn proper-split [edges]
  (let [splits (-> (apply graph/remove-edges fullg edges)
                   graph.alg/connected-components)]
    (when (= 2 (count splits))
      (* (count (first splits)) (count (last splits))))))

(defn run []
  (graph.io/view fullg :alg :neato :fmt :svg)
  (proper-split [["fdb" "psj"] ["nqh" "rmt"] ["ltn" "trh"]]))
