(ns advent.2019.day13
  (:require [advent.2019.grid :as grid]
            [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def memory
  (mapv #(Integer/parseInt %) (-> (io/reader "resources/2019/day11.input")
                                  slurp
                                  (str/split #","))))

(defn read-tile [out-chan]
  (when-let [x (async/<!! out-chan)]
    [[x (async/<!! out-chan)] (async/<!! out-chan)]))

(defn read-tiles [out-chan tiles]
  (loop [tile (read-tile out-chan)]
    (when tile
      (println "tile" tile)
      (swap! tiles assoc (first tile) (last tile))
      (println "check" (get @tiles (first tile)))
      (recur (read-tile out-chan)))))

(defn display [tiles]
  (grid/print (dissoc tiles [-1 0])
              (fn [tile]
                (when (= tile 4) (println "ball"))
                (get {0 " "
                  1 "|"
                  2 "B"
                  3 "-"
                  4 "*"} tile))
              {:default 0
               :empty-point " "
               :y-dir :top-down})
  (println (get tiles [-1 0] 0))
  (Thread/sleep 3000))

(defn position-of [tiles target-tile]
  (-> (filter (fn [[_ tile]] (= tile target-tile)) tiles)
      first
      first))

(defn move-paddle [ball paddle]
  (println "ball position in move-paddle" ball)
  (compare (first ball) (first paddle)))

(defn run []
  (let [tiles (atom {})
        in (fn []
             (let [tiles @tiles]
               (println "ball position" (position-of tiles 4))
               (display tiles)
               (println)
               (move-paddle (position-of tiles 4)
                            (position-of tiles 3))))
        out-chan (async/chan)
        f (future (intcode/execute-instructions :arcade
                                                memory
                                                in
                                                (intcode/chan->out out-chan)
                                                (intcode/halt-chans out-chan)))]
    (future (read-tiles out-chan tiles))
    @f
    (display @tiles)))
