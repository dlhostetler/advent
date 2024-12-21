(ns advent.2024.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [loom.alg-generic :as graph.alg]
            [loom.graph :as graph]))

(defn parse-code [line]
  (->> line
       seq
       (mapv str)))

(def codes
  (map parse-code (-> "resources/2024/day21.input"
                      io/reader
                      slurp
                      (str/split #"\n"))))

; +---+---+---+
;| 7 | 8 | 9 |
;+---+---+---+
;| 4 | 5 | 6 |
;+---+---+---+
;| 1 | 2 | 3 |
;+---+---+---+
;    | 0 | A |
;    +---+---+
(def numeric-edges
  {["A" "0"] :l ["A" "3"] :u
   ["0" "A"] :r ["0" "2"] :u
   ["1" "2"] :r ["1" "4"] :u
   ["2" "0"] :d ["2" "1"] :l ["2" "3"] :r ["2" "5"] :u
   ["3" "A"] :d ["3" "2"] :l ["3" "6"] :u
   ["4" "1"] :d ["4" "5"] :r ["4" "7"] :u
   ["5" "2"] :d ["5" "4"] :l ["5" "6"] :r ["5" "8"] :u
   ["6" "3"] :d ["6" "5"] :l ["6" "9"] :u
   ["7" "4"] :d ["7" "8"] :r
   ["8" "5"] :d ["8" "7"] :l ["8" "9"] :r
   ["9" "6"] :d ["9" "8"] :l})

(def numeric-keypad-graph
  (apply graph/digraph (keys numeric-edges)))

(defn numeric-path-part [[from to]]
  (graph.alg/bf-paths-bi (graph/successors numeric-keypad-graph)
                         (graph/predecessors numeric-keypad-graph)
                         from
                         to))

(defn numeric-path [code]
  (->> (concat ["A"] code)
       (partition 2 1)
       (map numeric-path-part)))

(defn numeric-path->numeric-directions [path]
  (-> (->> path
           (partition 2 1)
           (map vec)
           (mapv numeric-edges))
      (conj "A")))

(defn numeric-paths->numeric-directions [paths]
  (map numeric-path->numeric-directions paths))

(defn numeric-permutations [code]
  (->> (numeric-path code)
       (map numeric-paths->numeric-directions)))

;    +---+---+
;    | ^ | A |
;+---+---+---+
;| < | v | > |
;+---+---+---+
(def directional-edges
  {["A" :u] :l ["A" :r] :d
   [:u "A"] :r [:u :d] :d
   [:r "A"] :u [:r :d] :l
   [:d :u] :u [:d :r] :r [:d :l] :l
   [:l :d] :r})

(def directional-keypad-graph
  (apply graph/digraph (keys directional-edges)))

(defn directional-path-part [[from to]]
  (graph.alg/bf-path (graph/successors directional-keypad-graph)
                     from
                     to))

(defn directional-path [directions]
  (->> (concat ["A"] directions)
       (partition 2 1)
       (map directional-path-part)))

(defn directional-path->directional-directions [path]
  (-> (->> path
           (partition 2 1)
           (map vec)
           (mapv directional-edges))
      (conj "A")))

(defn directional-directions [directions]
  (->> (directional-path directions)
       (map directional-path->directional-directions)
       (apply concat)))

(defn shortest-sequence-dirs [directions]
  (->> directions
       directional-directions
       directional-directions
       count))

(defn shortest-sequence [permutations-seq]
  (->> permutations-seq
       (map (fn [permutations] (->> permutations
                                    (map shortest-sequence-dirs)
                                    (reduce min))))
       (reduce +)))

(defn complexity [code shortest-seq]
  (* (Integer/parseInt (apply str (butlast code)))
     shortest-seq))

(defn run []
  (->> codes
       (map numeric-permutations)
       (map shortest-sequence)
       (map complexity codes)
       (reduce +)))
