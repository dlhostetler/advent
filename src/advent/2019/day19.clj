(ns advent.2019.day19
  (:require [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]
            [advent.2019.grid :as grid]))

(def memory (intcode/file->memory "resources/day19.input"))

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

(defn deploy->grid [w h]
  (into {} (for [x (range w)
                 y (range h)
                 :let [point [x y]]]
             [point (deploy point)])))

(defn display [grid]
  (grid/print grid
              {0 "."
               1 "#"}
              {:default 0
               :empty-point " "
               :padding 0
               :y-dir :top-down}))

(defn run []
  (let [grid (deploy->grid 50 50)]
    (display grid)
    (->> grid
         vals
         (filter #(= 1 %))
         count)))