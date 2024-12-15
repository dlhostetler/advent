(ns advent.2024.day15
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def dir-to-fn
  {:n grid/north
   :e grid/east
   :s grid/south
   :w grid/west})

(defn parse-move [c]
  (if-let [dir (get {\^ :n
                     \> :e
                     \v :s
                     \< :w}
                    c)]
    dir
    (throw (Exception. (str "found invalid move character '" c "'")))))

(def input
  (-> "resources/2024/day15.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(def init-tiles
  (->> (-> input
           first
           (str/split #"\n")
           grid/lines->grid)
       (map-vals str)
       (remove (comp (partial = ".") val))
       (into {})))

(def moves
  (->> (str/replace (last input) #"\s+" "")
       seq
       (mapv parse-move)))

(def init-state
  (let [p (->> init-tiles
               (filter (comp (partial = "@") val))
               first
               first)]
    {:at p
     :moves moves
     :tiles (dissoc init-tiles p)}))

(defn find-space [tiles from dir-fn]
  (let [next (dir-fn from)]
    (cond
      ;; box
      (= (tiles next) "O")
      (recur tiles next dir-fn)

      ;; space
      (not (contains? tiles next))
      next

      ;; wall
      :else
      nil)))

(defn get-boxes [tiles from to dir-fn]
  (loop [next (dir-fn from)
         boxes []]
    (cond
      ;; at space
      (= next to)
      boxes

      ;; box
      (= (tiles next) "O")
      (recur (dir-fn next) (conj boxes next))

      ;; somethine else
      :else
      (throw (Exception. "trying to move get box, found something else")))))

(defn move-boxes [tiles boxes dir-fn]
  #_(println "moving boxes" boxes)
  (let [without-boxes (apply dissoc tiles boxes)]
    (merge without-boxes
           (zipmap (map dir-fn boxes) (repeat "O")))))

(defn move [{:keys [at tiles] :as state} to dir-fn]
  (-> state
      (update :tiles
              move-boxes
              (get-boxes tiles at to dir-fn)
              dir-fn)
      (update :at dir-fn)))

(defn try-move [{:keys [at moves tiles] :as state}]
  (let [next-dir (first moves)
        next-dir-fn (dir-to-fn next-dir)
        space (find-space tiles at next-dir-fn)]
    #_(println "trying to move" next-dir "found" space)
    (-> (if space
          (move state space next-dir-fn)
          state)
        (update :moves rest))))

(defn print-state [{:keys [at tiles]}]
  (grid/print (assoc tiles at "@")))

(defn do-moves [{:keys [moves] :as state}]
  #_(print-state state)
  (if (empty? moves)
    state
    (recur (try-move state))))

(defn gps [[x y]]
  (+ x (* 100 y)))

(defn run []
  (->> init-state
       do-moves
       :tiles
       (filter (comp (partial = "O") val))
       keys
       (map gps)
       (reduce +)))
