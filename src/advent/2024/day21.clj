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

(defn path-part [graph [from to]]
  (if (= from to)
    [nil]
    (graph.alg/bf-paths-bi (graph/successors graph)
                           (graph/predecessors graph)
                           from
                           to)))

(defn path [graph code]
  (->> (concat ["A"] code)
       (partition 2 1)
       (map (partial path-part graph))))

(defn path->directions [edges path]
  (-> (->> path
           (partition 2 1)
           (map vec)
           (mapv edges))
      (conj "A")))

(defn paths->directions [edges paths]
  (map (partial path->directions edges) paths))

(defn path-permutations [graph edges code]
  (->> (path graph code)
       (map (partial paths->directions edges))))

(defn shortest-sequence [graph edges robot code]
  (->> (for [part-permutations (path-permutations graph edges code)]
         (if (zero? robot)
           (count (first part-permutations))
           (->> (for [permutation part-permutations]
                  (shortest-sequence directional-keypad-graph
                                     directional-edges
                                     (dec robot)
                                     permutation))
                (reduce min))))
       (reduce +)))

(alter-var-root #'shortest-sequence memoize)

(defn complexity [code shortest-seq]
  (* (Integer/parseInt (apply str (butlast code)))
     shortest-seq))

(defn run []
  (->> codes
       (map (partial shortest-sequence
                     numeric-keypad-graph
                     numeric-edges
                     25))
       (map complexity codes)
       (reduce +)))
