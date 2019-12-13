(ns advent.2019.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]))

(def memory
  (mapv #(Integer/parseInt %) (-> (io/reader "resources/day11.input")
                                  slurp
                                  (str/split #","))))

(defn read-tile [out-chan]
  (when-let [x (async/<!! out-chan)]
    [[x (async/<!! out-chan)] (async/<!! out-chan)]))

(defn read-tiles [out-chan]
  (loop [tile (read-tile out-chan)
         tiles []]
    (if tile
      (recur (read-tile out-chan) (conj tiles tile))
      (into {} tiles))))

(defn run []
  (let [out-chan (async/chan)
        f (future (intcode/execute-instructions :arcade memory nil out-chan))
        tiles (read-tiles out-chan)]
    @f
    (->> tiles vals (filter #(= 2 %)) count)))