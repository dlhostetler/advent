(ns advent.2019.day19
  (:require [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]
            [advent.2019.grid :as grid]))

(def memory (intcode/file->memory "resources/2019/day19.input"))

(defn deploy [[x y]]
  (let [in-chan (async/chan 2)
        out-chan (async/chan 1)]
    (async/>!! in-chan x)
    (async/>!! in-chan y)
    (async/close! in-chan)
    (intcode/execute-instructions :drone
                                  memory
                                  (intcode/chan->in in-chan)
                                  (intcode/chan->out out-chan)
                                  (intcode/halt-chans out-chan))
    (async/<!! out-chan)))

(defn points [x y w h]
  (for [x (range x (+ x w))
        y (range y (+ y h))]
    [x y]))

(defn deploy->grid [x y w h]
  (->> (points x y w h)
       (pmap (fn [p] [p (deploy p)]))
       (into {})))

(defn display [grid]
  (grid/print grid
              {0 "."
               1 "#"}
              {:default 0
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(defn fits-width? [grid x y w]
  (->> (map (fn [x y] [x y]) (range x (+ x w)) (repeat y))
       (map grid)
       (every? #(= 1 %))))

(defn fits-height? [grid x y h]
  (->> (map (fn [x y] [x y]) (repeat x) (range y (+ y h)))
       (map grid)
       (every? #(= 1 %))))

(defn fits? [grid x y w h]
  (and (fits-width? grid x y w)
       (fits-height? grid x y h)))

(defn fit [grid x y w h target-w target-h]
  (->> (points x y w h)
       (filter (fn [[x y]] (fits? grid x y target-w target-h)))
       first))

(defn run [x y w h]
  (let [grid (deploy->grid x y w h)]
    (display grid)
    (fit grid x y w h 100 100)))