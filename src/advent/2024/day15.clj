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

(defn widen-tile [tiles [[x y] tile]]
  (cond
    (= tile "#")
    (-> tiles
        (assoc [(* x 2) y] "#")
        (assoc [(inc (* x 2)) y] "#"))

    (= tile "O")
    (-> tiles
        (assoc [(* x 2) y] "[")
        (assoc [(inc (* x 2)) y] "]"))

    :else
    (assoc tiles [(* x 2) y] tile)))

(def init-tiles
  (->> (-> input
           first
           (str/split #"\n")
           grid/lines->grid)
       (map-vals str)
       (remove (comp (partial = ".") val))
       (reduce widen-tile {})))

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

(defn try-move-box-west [tiles [x y]]
  (let [next-tile-at [(- x 2) y]]
    (cond
      ;; there's a space
      (not (contains? tiles next-tile-at))
      (-> tiles
          (dissoc [x y] [(dec x) y])
          (assoc [(dec x) y] "]" [(- x 2) y] "["))

      ;; there's a box
      (= (tiles next-tile-at) "]")
      (if-let [moved-tiles (try-move-box-west tiles next-tile-at)]
        ;; could move that one, so move this one
        (-> moved-tiles
            (dissoc [x y] [(dec x) y])
            (assoc [(dec x) y] "]" [(- x 2) y] "["))
        ;; couldn't move
        nil))))

(defn try-move-box-east [tiles [x y]]
  (let [next-tile-at [(+ x 2) y]]
    (cond
      ;; there's a space
      (not (contains? tiles next-tile-at))
      (-> tiles
          (dissoc [x y] [(inc x) y])
          (assoc [(inc x) y] "[" [(+ x 2) y] "]"))

      ;; there's a box
      (= (tiles next-tile-at) "[")
      (if-let [moved-tiles (try-move-box-east tiles next-tile-at)]
        ;; could move that one, so move this one
        (-> moved-tiles
            (dissoc [x y] [(inc x) y])
            (assoc [(inc x) y] "[" [(+ x 2) y] "]"))
        ;; couldn't move
        nil))))

(declare try-move-box-vertical)

(defn try-move-box-half-vertical [tiles [x y] tile inc-or-dec]
  (let [next-tile-at [x (inc-or-dec y)]]
    (cond
      ;; there's a space
      (not (contains? tiles next-tile-at))
      (-> tiles (dissoc [x y]) (assoc next-tile-at tile))

      ;; there's a box
      (or (= (tiles next-tile-at) "[") (= (tiles next-tile-at) "]"))
      (if-let [moved-tiles (try-move-box-vertical tiles next-tile-at inc-or-dec)]
        ;; could move that one, so move this one
        (-> moved-tiles (dissoc [x y]) (assoc next-tile-at tile))
        ;; couldn't move
        nil))))

(defn try-move-box-vertical [tiles [x y] inc-or-dec]
  (let [x0 (if (= (tiles [x y]) "[") x (dec x))
        x1 (inc x0)]
    (if-let [left-tiles-moved (try-move-box-half-vertical tiles [x0 y] "[" inc-or-dec)]
      (try-move-box-half-vertical left-tiles-moved [x1 y] "]" inc-or-dec)
      nil)))

(defn try-move-box [tiles box-at dir]
  (let [tile (tiles box-at)]
    (cond
      ;; west
      (and (= dir :w) (= tile "]"))
      (try-move-box-west tiles box-at)

      ;; east
      (and (= dir :e) (= tile "["))
      (try-move-box-east tiles box-at)

      ;; north
      (and (= dir :n) (or (= tile "[") (= tile "]")))
      (try-move-box-vertical tiles box-at dec)

      ;; south
      (and (= dir :s) (or (= tile "[") (= tile "]")))
      (try-move-box-vertical tiles box-at inc)

      :else
      (do
        (grid/print tiles)
        (throw (Exception. (str "try-move-boxes, moving " dir " with " tile)))))))

(defn try-move [{:keys [at moves tiles] :as state}]
  (let [next-dir (first moves)
        next-dir-fn (dir-to-fn next-dir)
        next-at (next-dir-fn at)]
    #_(println "trying to move" next-dir "to" next-at)
    (cond
      ;; space
      (not (contains? tiles next-at))
      (-> state
          (assoc :at next-at)
          (update :moves rest))

      ;; box
      (or (= (tiles next-at) "[") (= (tiles next-at) "]"))
      (if-let [moved-tiles (try-move-box tiles next-at next-dir)]
        ;; boxes-moved
        (-> state
            (assoc :tiles moved-tiles)
            (assoc :at next-at)
            (update :moves rest))
        ;; couldn't move boxes
        (update state :moves rest))

      ;; can't move
      :else
      (update state :moves rest))))

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
       (filter (comp (partial = "[") val))
       keys
       (map gps)
       (reduce +)))
