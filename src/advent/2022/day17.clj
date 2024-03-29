(ns advent.2022.day17
  (:require [advent.seq :as seq]
            [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def rocks
  [[[0 0] [1 0] [2 0] [3 0]]
   [[1 0] [0 1] [1 1] [2 1] [1 2]]
   [[0 0] [1 0] [2 0] [2 1] [2 2]]
   [[0 0] [0 1] [0 2] [0 3]]
   [[0 0] [1 0] [0 1] [1 1]]])

(def pushes
  (->> "><<<<><<>>><<<>>><<><<>>>><<><<>><><<<<>>>><<>>><<<>>>><<<<><<<<>>>><>>>><>>><<><>><<>><<<<>><<><<><>><<><>>><<<>><>>><<>><<>>><<<><>>><<>>><<<<>><<<<><<<><<>>><<<>>><>>><<<>>><<<<>>>><<<>>><<<>><<<<>><<<>>><<<>>><<>><<>><<<>><>><<<><<>><<<>>>><>>>><>><<>>>><<<<><<<<><<<<><<>>>><<<>>>><<<><<>>><<<><><<>><<>>><<>><>>><<>><<>>>><<<<><<<>>><<>>>><<>>>><<>><>>><<<<>>><<<>><<<<><>>><>><<<<>>><<>>><><<<>>><<<><<<>><<<<>>>><<<><<<><<>>>><<<>>><<><<>>>><<<<>><<>><<<<>>>><<<><<>>><<<<>>>><<<<>>>><<<<>><><>><<>>>><>>>><<<<>><>>><<<<>>><><<><<<>>>><><<<>><<>>><<<>>><<<>><<<><<>><<><<>>>><<>>><<<<><<<<><>>>><<>><<>>><<<>>>><<<<>>>><<<>>>><<>><>>>><><<<>>>><<<<><>>><>>><<>>>><>>><<<<><<>>><<<<><<>>><<>><<<>><<<<>>><<>>><<<>>><>>><<<<><>>><><<<<>>><>>><<<<><><<<<>>>><<>>><<<<>>><<<<>>>><>>>><<><<<<>>><<<<>><<<<>><<>><<<<>>>><<<>>>><<<<>>><>>>><<<<>><<<<>><<>>>><<<><<<>>>><<><<<<>>>><><<<<>><>><<>>>><<<<>>><<>>><<>>><<<>>>><>>><<<<>>>><>>><<>><<><<>><<><<><<<>>><>><<<>><<<><<<<><<<<>><<>><<<<>>>><<<<><<<<>>>><<><<><<<<>><<<>>><>>><<<<><><>><<>>><<><>>><<<>><<<<><<<<>>><<<>>>><<>><<><<<<>><<<<>>><<>><<>><<<<><>>>><><<>>>><>>><<<><>><<>>>><>>><<<<><<<<>>><<<>>><<<>>>><<<<><<<<>>>><<><<<<>>><<<>><<>>>><<>>><<>><<><<<<>>><<>><<><<<<>>><<<><<<<>>><<>>>><<<<>><>>>><<<<>>><<>>><<><<>><<<>>><<<<>>>><<<<>><<><<>><<<<>>>><><<<><><<<><<<><<<<>><>><<<>>><><<<><<<<><<<>><>>><<>><>>><<>><<>><><>>>><<<><>>><<>>>><<<>><<<>><<<<>>><<<<>>><<<<>>>><>><<<>><<>><<<><<>>>><>><<<><<<<>>><<><<<<>><<>>><<<<>>>><<<>>>><>>><<<>>><<<>>><<>><<<<>>>><<<<><><<<<>>>><<<>><<<<><<<<>>><<<<>>>><<>>><<<<>>>><<<<><<<><>>>><<>>>><<<<>><<<>>>><<<<>><<>>>><>><>>>><<<>><<>>>><<<>>>><<>><<<>>>><<<>><>><><<><>><>><<<>><<<>>><<>>><<<<>>><<<><<>>><<<<><>>>><<><>>>><<><<<>>>><><<<<>>><<<<>><<>>>><<<><<<>><<><>>><<>>>><<><>>>><<<<><>><<>><<<>>><>><<<>><<<<>>><<<<>><>><<><<<<><<<>>><><>>><<>>>><<><<<<>>><<>>><<<><<>>><><<<><>>><<><<<>>><<>>><<><<><<<<>>><<><<<<>><<<<>><>>>><<<<><>>>><<<<>>>><<>><<<<>>>><<<<><<<>>><<<<>>><<<<><<<><<>><>>>><>>><<<>>>><<<<>>><<<>><<<<>>>><<<>>><<<<>>><<<<>>><<<>>><<>><<<>><<<<>><<<<>>>><<>><<>><>>><<<<>>><<<<>>><<<<><<>>><><<<<>><<>>>><<><<>>><>><<<><>>><<<<><<>><<>>><><<>>><<<<><<<<><>>>><<>>><<>>>><<<>>><<>>><<<<>>><<>>>><>><><<<<>>>><>>><<<><<<<>>><<><<>>>><<<<>>>><>><>><<<>>>><>><><<>>><<<<><<<>>><<>>><><<>>>><<>>>><<<><<<>>><<<><<<>>><<<><<>>><<<<>><<<>><>>><>><<<<>>>><><<<<>>>><><<<<>>>><<<>>>><<>><<<>>><<<<>>>><<<>><<><>>>><<<>>><<<<><<<>>><<<>>><<>><<>>>><<>>><>><<<>>>><<>>>><<<><<>>><<<>><><<>>><<<<>>>><<<><<>>><>><>>>><><>>><><<<<>><<<><>>>><>><<<>><>>><<<><<><><<>><<<>>>><<<<>>>><>>>><<<><<<>><>>>><<>>>><<<>>><<><<<<><<<<>><<<>><<>>><<><><<<<>>><>>>><<<><>><<><>><>>>><><>>><<><<>><<<>>>><<>><<><<<><><<<>>>><<<<>><<<<>>><<<<>>>><>>><<<<><<>>>><><<>>><<<<>><<<>><<<>>>><><<<>><<<<><<<><<>>>><<<>>>><<<<>>><<<>>>><<<<>><<<>>><<<>>><>>>><<<<><<<<>><>>>><<>><<<<>>><<<<><<<<><<<<><<<<>>><<<>><<<>><<>>>><>><<<<><<><<>>><<>>>><<<><>><<<<><<<>>>><<<<><>><<<>>>><<<<>>>><<<>><<<><<<>>>><<>>>><<<>><><<<<>>>><<>>>><<<>>><>>>><<<<>><<<<>><>>><<<<>>><>>>><<<<>>><<>>><<<>>><<<>>><<<<>>><<<<>>>><<<<>><<<<>><>>><<<<>>><<<>>><<>><<<<><<>>><>>>><<<>>><<>>><<><<<>>>><<<>>><<>>>><<>>><><<<<>><>>>><>>><>>>><<<>><<>><<<<><<<><>><<<<>>>><<<><<>><<<<>><<>><<>><<>>><<<<>>><<<<><<<>>><>><>>>><<<>>><>>>><<<<>><<<<>><>>>><<>>><<<<>>><<>>><<<<><>>>><>><><<<<>><>>><<<<>><<<<>>>><<<>>>><<<<>><<<<><<<<>>><>>><<<<>><<<><<<><><<<>>><<>>>><<<<><<<<>>><<<<><<>>><<<>>><<<<>>>><<<>>><<<<><<><<>>>><>>>><<<<>>><<<<>>>><<>>>><<<>><<<<>><<>>>><<<>>>><<<<>>><<<<>>><<<><<><<<<>>>><<><<><>>>><<<>>><<<<>>><>><>>>><<<<>><<<>>>><<<>><>>><<>><>><<<<>>>><><<<>>>><<>>>><<<<><<<>>><<<<>><<<<>>>><<<><<<>><<>><<<>>>><<<>><>>>><<>>>><<<>><>><>>>><<<>>><<<<>>>><>><<>><><>>><<<<><<>><>><>>><<>><>><<<><<>>>><<>>>><<<>>>><<>>>><><<>>>><<<>><<<<>><<<<><<<<>>>><>>>><><>>><<<<>>>><<<>><<<>>><<<<>>><<><<<>>>><>>><>>><<<>>><<<><>>><<<><<<>>>><<>>><<<<>><<<<>><<<>><<<<>>><<<<>>>><<<<>>>><>>>><><<<<>><<<<>><<<<>>><<<<>>>><>>><<>><>>><>>>><>>><<<><>><>>><><>>><>><>>>><<<<>><<<>>>><<<>>><<<<>><<<<>>><>><><<<<>><<>><<<><<>><<<<>>><<<>><>><>><<><<<>>>><<<<>><<<<><<<<><<<>>>><<><<<<>><<<>>><<><<><>>><<>><<>>>><<<>><<>><>><<>><<<>>>><>>>><><>>>><<<<>>>><<>>>><<>>>><<<<><<>><><<<<>>><<>>>><<<<>>><>>>><<<>>>><<<>><<>>><<>>><<<>>><<<<>>><<><<<<><<<<>>><<>>><<<<>>><<<>>>><<<<>><<<<>>>><<<<>>>><<><>><<>>><<>><<<>>>><>>><>><<<<>>><>><<<<>>>><<<<>><><<<><>>>><><<>>><<<<>><<<<>>><>><>>><<<>>><><<<<>>><>>><<<<>>><>>><<<>><>>>><><<<<>>>><<<<>><<<><<<>><<<>>><<<>>><<>>><>>><<<<>><>>><>>><<<>><<>>><<><<<<>><>>>><>>>><<<>><><>>><<<><>><<<>>>><<>>><<<<>>>><<>>><<<<>><<<<>>>><<<<>>>><<>>><>>>><<<<>>>><<<>>>><<<<>>><<>><><>>><<<>>>><<>>>><>>><<<<>>><<>>><<>>><<<>><>>><>>><<><<<><<<><>><><>>>><<<>>>><<<<>>>><>>><<<>>><<<<>><<<<><<>>><<>>>><<<<>>><<<<>>>><<>><<<<>>><<<<>><<<>>><<>>>><<>>>><<<<>>><<>><>><>><>><<<>>>><<<<>>>><<<>>>><<<<>>>><<<<><<<>><<<>><<<><<>>><<<>><>><<>>><<<><<><<<>>><<<<>><><<>>><<>>>><<><<<>>>><><<<>>>><<<<>><<<>>>><<>>><<<<>>><<<<>>><<<<>>>><<>>>><<><<<<>><<><<>>>><<><<>>><<<>>>><<>>><<<<>>><<<>>><<>>>><<<>>>><<<<>>><<<<>>><<<<>>><<><<>>><<<<>><<<<>>><<<><<<>>>><<<<>>>><<><><<>>><<<<><<<>>>><<<>>>><<<<>>><>>><<>>>><>><<<<>>><<<<>>>><<>><<<<>>><<<>>>><<<>>>><<<<>><>>>><<<<>>>><<<>><>>><<<<><<<>>><<<<>>>><<<<>>><<>>>><<>>>><<<>>><<<<>><>>><<<<>><<<<>>>><>><<<>>>><<><<>>>><<<>>><<><>>>><<<>>><<<>>>><<<<><<<>><<<<><<<>>>><<<<>>>><<<>>>><<<>>><>>>><<<><>>><<<<><<<><>><>><>><<<<>>><<><<<>>><<>><<<<>>><<<<>>><<<<>>><<><<<<>>>><<>>><<<>><<<>><>><<<>>>><>>>><<<<>><<><<<>><<<<>><<>>><<<>>>><<<><><<<>>><><<<<>>><<>><<>><>><<<<>><<<<><>>>><><>>>><<<><<><<<>>><>>><<<>>><<<<><<<<><><<><<>><<>><<<<>>><>>>><<<<>><<><>><>>><>>>><<<>>><<><>><<<<><>>>><>>>><>>>><<>>>><<<>>><<>>><<<>>><>>>><<>><<>><<<<>>>><<<>>><<>><>><<<>>>><>>><<<>>><<<>>><<<<>>>><<><>><<<>>><<<>>>><>>><<><>><<<<>><<<<>>>><<<<>>>><<<>><<<<><>>>><<><><<>><>><><>>>><<<>><<<<><<<<><><><>>><>><<<<>>>><<>>>><>>>><<>><>>>><<<<>><<<<>>><><<<>><>>>><>>>><<>>>><<<<>>>><><<<<><><<<<><<<<><<<>><<<>>>><<<<>>>><>>>><<<<>><<>>>><<<<>>>><>>><<<>>>><<><>>><<<<>>>><<><<><<<>><>><<><<>><<<<>>>><>>>><<>>><<<<>>><>>>><<<>>><<<>><<<><<<><>>><>><>><<<>>>><<<<><>>>><<<<><<<<><<>>><<<>>><<<>><>>><<<>><<<>><<><<<<>><<<>>>><<<>>>><>>><<<>>>><>>>><<<><<>><>><<<<><>>>><<<<>><>>>><<<><<<<><>>><<<<>>>><<<<>>>><<<>><>><<<>>><<<><<<<>><<><>>><<>>>><>><><<<<>>>><<<<>>><<>><<><<<<><<<<>>><<<>><<><<><>>>><>><<<<><<<><<<<>><<<<>><<<>><<><<<<><<<>>>><<><<<<>><<<<>>>><>>><<<<><<<>>><>>>><>>>><<<>>>><<>>>><<>>><<>>>><<>><<<<>>><<<>>><<<<>><<><<>>>><>><<>>>><<<<><<<>>>><<<>>>><<<>>>><<<><<<>><>>><<<<>>><<>><<<><<>>>><<<><<>>>><<<<>><<<<>><>><><<<>><<>>>><<<>><<<<>>><<<><<<<><<<>>><<<><<<<>>>><<>>>><>><<<<>>><><<<>>><<>>>><<<<><<>><<>>><>><<<>>>><<<<>>>><<>>>><<<<><><<<<>><><>><>><<>>><<>>><<><<<><><<<><><<>>><<<<>>>><<<>>><<<>><<<<>><<<>>><<<<><>>>><<>>>><<<<><<<>><<<>>>><<>><<>>>><<<>><<><<<>>>><>>><<<>>>><<>><<<<>>><<>><<<<><<>>><<<<>>>><>>>><<<>>><>><>>>><>>>><<>>><>>>><>>><<>>><<>>><<<>>><<<<>><>>>><<><<>>><<<>>>><<<<>>><<<><<<>>><<<>>><<>><>>><><<<><<>>><<>><<<>><<>>>><<>>>><>>>><<<>>><<<>>>><<<<>>>><><<<<>><<<>><<><<<<>><<<<><>>><>><>>><>>>><<>>><>>><<<<><<<<><<<>><>>>><>><<>>><><<<>>>><<<>>>><>>>><<<<>>><<<<>>>><<<>>><<<><>>>><><<<>><>>>><<><<<><<<>>><<>>><>>><<<<>>>><<><<<<>>>><<>>><<<<>>>><<<><<<<><>>><<<><>>><<<<>>><<<<><><<<<>><<<>><<<<><<<>><>>><>><<>>>><<>>><<<<>>><>>><<><<<<>>><>><<<<>>><><><<>>>><<<>>>><<>>>><<<<><>>>><<<<>><<>>>><>><<<>>><<>><>>><<<><<<>><<<><<>>><>>><<><<>><<<<><<>>>><<>>>><<<>>><<<<>><<<>><<<>>><<<>><<<<>>><<<<><><<<><><<<>><><>>>><>>>><<<><<<><<<<><<<>>>><<>><>>>><<<<>>>><>><<<<>>><>>>><<<><<<>>>><<<>>><><<<>>>><<<>>><<<<>>><>>><<<<><<<>><<<><><<<>><<><>>>><<>><<>>><<>><<<><>>>><<>><<<>><<>><>>><<<><<<<>>><<>>>><<>>><<<<>>><>>>><<<<>>><<<<>>><<>>><<>>>><<<<><>><<<>>><<<>>><>><<>>><><>><<>>><<<<>><<<<><<<<>>><<<<>>><<>>>><<<<>>>><>>><<<<><<>>>><<>>><>><<<<><<<<>>><<>>><<<<>>><<<><<>><<<<>>><>>>><<<>>>><<>><>><<<><<<>>>><<>>><<<<>>>><<<<>><<<<>>>><<<>>>><<>>><<<<>>><<<>>>><<>>><><>>>><<<<>>><<<<>>><<<>>>><<<<>>>><>>>><<>>>><><<>><>>>><<<<>>><<<<><<<>><>><<>>>><<>><>>><<>><<>>><<>>><<>>>><<<<>><<<<>><<<<>>>><<<<>>>><<>><<<><<>>><<>>><<<<>>><>>>><<><>><><>>><<<>><<<<>>>><<>><<<><<<>>><<<<>>><<>>>><<>><<<>>><<>>>><<>><<>>><<<<>><<<<>><<<><<<>>>><<<>>><<<<>><>><>>><>>>><>>>><<>>>><>>><<<>>><<><>>>><<<<>>><<<>><<<<>><<><>>>><<<<><<><>>><<<<>><<<<>>><<<><<><<<>><>>>><<<<>><<<<>>>><<<>>><><<>>>><<><><<>>><<<<>><<<<>>><>>>><<<>><<<<>>>><<<>>><<<<>>>><<><<<>>><<>>><<<><><<<<><><<>><<<<>>>><<<<>><<<<><<>>>><>><>><<>>>><>>>><<<<>><><<<>><<<>>>><<><<>><<<<>><<><>>>><<<<>><<>><<<>>><>>>><<<<><<<<>><<<><<>>>><<<><<<>>>><<>>>><<<<><>>><<<>>>><<<<>>>><>><>>>><<<>><<>>>><<<>>>><>>>><<<<>>>><<<<>>><<<><<><<<<>><>>>><<<>>><<>><<<<>>><<<<>><>><>>>><>>>><<>><>>>><<<<>>><>>><<<<>><<><<<>>><<>>>><<<<>>>><><><<<>>><<<<><<<<>><<><<<<>><<<>>><<><<<<>>>><<<<>><>><>><><><<><>><<<<>>><<><<<>>><<><<>>>><<<><<<<><<<<><<>>><>>><<>>><<>>>><<><<<<>>>><><<><<>>><><<<<>><>>><><<<<>>><<<<>><<<>>>><<<<><<<>><<<>><<><><<<>><<<>>>><<<><<<><>>><<<<>>><<>><<<>>><<<>>><<<><<><<<<>>><<<><>>>><<><<<>>><>>><<<>>><<<>>>><<<>>><<<>>>><<<><<>>><<>><<<>>>><<<<>><<<<><><<<<>><<>><<<>>><<<>><<<>>><<<<>><<>><>><<<><<>>><<<>>><>>><<<>>><<<>><<<>>>><<<<>><<>><<>><><<<>><<<<>>><<<>>><<<<><>>><<<<>><<<<>>>><<<>>><<<<>><<>>><<>>><<<>>><<<>><<>>>><><<>>>><><<>>><><<<<>>>><<<<>><>><<<<>>><<<>><<<><<>>><<<<><>>><>>><<>>>><<><<<>><<<>>><<<>>><<<>><<>><<><<<<><<<<>>><<<><<<<><<<<><<<><<<><>>><<>>>><>>>><>>><<><<<<><<<>>><<<>>><<><<<<>>>><<<<>>>><<>>>><<>>>><<<<><<>><>><<<><><><>><<<>><<<<>>>><<<<>><<<<>>><<><<<<>><<><>>><<<>><<<><<<<>><><<<<><<<>>><>>><<>>><<>>>><><<>><>>>><<<<><<<<><<><<<>><<<>>>><<>><<>>>><<<<>><<<>><<<><<>><<>>><<>>><<>>><<>>>><<>>><<<<>>><<><<<<>><>><<<><<<>>><<>>>><<<>><>>><<><<<<>>>><>>>><<><>>><<<<><<<><>>>><<<<>>>><<<><>><<<<><<<>>>><<<><<<<>>><<<>><><>><>>>><<<<>>>><>><<<<>>><<>><<<<><<>>>><<<<>><>>><>><<<>>>><>><<<><><<>>>><<<>><<>><>><<><<>><>>><>>><<><><<>>>><<>>>><>>><<>>>><>><"
       seq
       (map str)
       (map keyword)
       cycle))

(def init-state {:pushes pushes
                 :falling nil
                 :grid {[0 0] :nothing
                        [6 0] :nothing}
                 :num-rocks 0})

(defn falling-rock [i]
  (->> (nth rocks i)
       (map (fn [point] [point :falling]))
       (into {})))

(alter-var-root #'falling-rock memoize)

(defn display [state]
  (grid/print (merge (:grid state) (:falling state))
              {:falling "@"
               :nothing "."
               :rock "#"}
              {:default :air
               :empty-point "."
               :padding 0}))

(defn rock? [[_ v]]
  (= v :rock))

(defn origin [grid]
  (let [y (->> grid
               (filter rock?)
               (map first)
               (map last)
               sort
               last)]
    [2 (if (nil? y) 3 (+ y 4))]))

(defn translate [grid [tx-x tx-y]]
  (into {}
        (for [[[x y] v] grid]
          [[(+ x tx-x) (+ y tx-y)] v])))

(defn add-rock [{grid :grid num-rocks :num-rocks :as state}]
  (let [rock-index (mod num-rocks (count rocks))]
    (-> state
        (assoc :falling
               (translate (falling-rock rock-index) (origin grid)))
        (update :num-rocks inc))))

(defn moved? [grid shape]
  (every? true?
          (for [[x y] (keys shape)
                :let [current (get grid [x y])]]
            (and (grid/valid-x? grid x)
                 (>= y (grid/min-y grid))
                 (or (nil? current)
                     (= current :nothing))))))

(defn translate-rock [{grid :grid :as state} translation]
  (let [next-falling (translate (:falling state) translation)
        m? (moved? grid next-falling)]
    (-> state
        (?> m? (assoc :falling next-falling)))))

(defn push-rock [state]
  (let [dir (-> state :pushes first)]
    (-> state
        (?> (= dir :<) (translate-rock [-1 0]))
        (?> (= dir :>) (translate-rock [1 0]))
        (update :pushes rest))))

(defn set-rock [{falling :falling :as state}]
  (-> state
      (update :grid merge (map-vals (constantly :rock) falling))
      (assoc :falling nil)))

(defn drop-rock [state]
  (let [next (translate-rock state [0 -1])]
    (if (= next state) (set-rock state) next)))

(defn move-rock [state]
  (-> state push-rock drop-rock))

(defn next-state [state]
  (if (nil? (:falling state))
    (add-rock state)
    (move-rock state)))

(defn height [state]
  (->> state
       :grid
       (filter rock?)
       keys
       (map last)
       (reduce max 0)
       inc))

(defn growth [[from to]]
  (- to from))

(defn calc-growths [n]
  (->> init-state
       (seq/successive next-state)
       (drop 1)
       (filter (comp nil? :falling))
       (take n)
       (map height)
       (partition 2 1)
       (mapv growth)))

(defn find-cycle
  ([growths] (find-cycle growths 0 1))
  ([growths start n]
   (if (> start 500)
     (do
       (println "got to" (str "start=" start))
       nil)
     (let [p (->> growths
                  (drop start)
                  (partition n))]
       (cond
         (= (count p) 1)
         (recur growths (inc start) 1)

         (apply = p)
         {:start start
          :start-height (reduce + (take start growths))
          :cycle (first p)}

         :else
         (recur growths start (inc n)))))))

(defn cycle->height [num-rocks {cycle :cycle
                                start :start
                                start-height :start-height}]
  (let [cycle-n (count cycle)
        cycle-height (reduce + cycle)
        cycled (long (/ (- num-rocks start) cycle-n))
        leftover (mod (- num-rocks start) cycle-n)]
    (+ start-height
       (* cycled cycle-height)
       (reduce + (take leftover cycle)))))

(defn run []
  (->> (calc-growths 5000)
       find-cycle
       (cycle->height 1000000000000)))
