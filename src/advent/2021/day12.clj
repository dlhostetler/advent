(ns advent.2021.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [loom.graph :as graph]
            [plumbing.core :refer :all]))

(def input
  (->> "resources/2021/day12.input"
       io/reader
       line-seq
       (map #(str/split % #"-"))))

(defn small-cave? [cave]
  (and (not= cave "start")
       (not= cave "end")
       (every? (fn [^Character x] (Character/isLowerCase x)) cave)))

(defn start? [node]
  (= node "start"))

(defn end? [node]
  (= node "end"))


(defn visited? [path node]
  (some #(= node %) path))

(defn find-paths! [g path paths]
  (doseq [successor (graph/successors g (last path))
        :when (not (start? successor))
        :when (or (end? successor)
                  (not (small-cave? successor))
                  (not (visited? path successor)))]
    (if (end? successor)
      (swap! paths conj (conj path successor))
      (find-paths! g (conj path successor) paths))))

(defn run []
  (let [paths (atom #{})
        g (apply graph/graph input)]
    (find-paths! g ["start"] paths)
    (count @paths)))
