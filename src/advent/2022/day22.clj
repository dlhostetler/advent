(ns advent.2022.day22
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (->> "resources/2022/day22.input"
       io/reader
       slurp))

(def grid
  (->> (-> (str/split input #"\n\n")
           first
           (str/split #"\n")
           grid/lines->grid)
       (map-vals {\. :open
                  \# :rock
                  \@ nil
                  \space nil})
       (remove (comp nil? last))
       (into {})))

(defn parse-int [i]
  (try
    (Integer/parseInt i)
    (catch Exception _ i)))

(def path
  (->> (str/split input #"\n\n")
       last
       (re-seq #"([0-9]+|[LR])")
       (map first)
       (map parse-int)
       (map (fn [x] (if (int? x) x (keyword x))))))

(defn display [grid]
  (grid/print grid
              {:here "@"
               :nothing " "
               :open "."
               :rock "#"}
              {:default :nothing
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(def start-at
  (->> grid
       keys
       (filter (comp zero? last))
       sort
       first))

(defn turn [facing l-or-r]
  (get (if (= l-or-r :R)
         {:east :south
          :north :east
          :south :west
          :west :north}
         {:east :north
          :north :west
          :south :east
          :west :south})
       facing))

(alter-var-root #'turn memoize)

(defn walk-east [[[x y :as at] facing]]
  (cond
    ;; 2->4
    (and (= x 149))
    [[99 (- 149 y)] :west]
    ;; 3->2
    (and (= x 99)
         (<= 50 y 99))
    [[(+ y 50) 49] :north]
    ;; 4->2
    (and (= x 99)
         (<= 100 y 149))
    [[149 (- 149 y)] :west]
    ;; 6->4
    (and (= x 49)
         (<= 150 y 199))
    [[(- y 100) 149] :north]
    :else
    [(grid/east at) facing]))

(defn walk-north [[[x y :as at] facing]]
  (cond
    ;; 1->6
    (and (<= 50 x 99)
         (= y 0))
    [[0 (+ x 100)] :east]
    ;; 2->6
    (and (<= 100 x 149)
         (= y 0))
    [[(- x 100) 199] :north]
    ;; 5->3
    (and (<= 0 x 49)
         (= y 100))
    [[50 (+ x 50)] :east]
    :else
    [(grid/north at) facing]))

(defn walk-south [[[x y :as at] facing]]
  (cond
    ;; 2->3
    (and (<= 100 x 149)
         (= y 49))
    [[99 (- x 50)] :west]
    ;; 4->6
    (and (<= 50 x 99)
         (= y 149))
    [[49 (+ x 100)] :west]
    ;; 6->2
    (and (= y 199))
    [[(+ x 100) 0] :south]
    :else
    [(grid/south at) facing]))

(defn walk-west [[[x y :as at] facing]]
  (cond
    ;; 1->5
    (and (= x 50)
         (<= 0 y 49))
    [[0 (- 149 y)] :east]
    ;; 3->5
    (and (= x 50)
         (<= 50 y 99))
    [[(- y 50) 100] :south]
    ;; 5->1
    (and (= x 0)
         (<= 100 y 149))
    [[50 (- 149 y)] :east]
    ;; 6->1
    (and (= x 0)
         (<= 150 y 199))
    [[(- y 100) 0] :south]
    :else
    [(grid/west at) facing]))

(defn forward [grid [at facing :as current] n]
  #_(println "moving" facing "from" at)
  (if (zero? n)
    current
    (let [[to :as next] (case facing
                          :east (walk-east current)
                          :north (walk-north current)
                          :south (walk-south current)
                          :west (walk-west current))]
      (when-not (get grid to)
        (display (assoc grid at :here))
        (throw (Exception. (str "invalid to " to))))
      (if (= (get grid to) :rock)
        current
        (recur grid next (dec n))))))

(defn walk-path [grid current n-or-turn]
  (if (int? n-or-turn)
    (forward grid current n-or-turn)
    (update current 1 turn n-or-turn)))

(defn password [[[x y] facing]]
  (+ (* (inc y) 1000)
     (* (inc x) 4)
     (->> facing
          {:east 0
           :north 3
           :south 1
           :west 2})))

(defn run []
  (->> path
       (reduce (partial walk-path grid) [start-at :east])
       #_display->
       password))
