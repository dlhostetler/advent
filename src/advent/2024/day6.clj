(ns advent.2024.day6
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [plumbing.core :refer :all]))

(def input
  (->> "resources/2024/day6.input"
       grid/slurp
       (map-vals str)
       (remove (comp (partial = ".") last))
       (into {})))

(def dir-to-fn
  {:n grid/north
   :e grid/east
   :s grid/south
   :w grid/west})

(defn init-state []
  (let [at (->> input
                (filter (comp (partial = "^") val))
                first
                first)]
    {:at at
     :dir :n
     :positions input}))

(defn inside? [positions at]
  (some? (grid/valid-point-or-nil positions at)))

(defn next-state [{:keys [at dir positions] :as state}]
  (let [next-at ((dir-to-fn dir) at)]
    (cond
      ;; already outside
      (not (inside? positions at))
      state
      (= "#" (get positions next-at))
      ;; turn
      (let [next-dir (case dir
                       :n :e
                       :e :s
                       :s :w
                       :w :n)]
        (assoc state :dir next-dir))
      ;; forward
      :else
      {:at next-at
       :dir dir
       :positions (assoc positions at "X")})))

(defn continue? [{:keys [at positions]}]
  (inside? positions at))

(defn run []
  (->> (init-state)
       (seq/successive next-state)
       (drop-while continue?)
       first
       :positions
       (filter (comp (partial = "X") val))
       (count)))
