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

(defn read-tiles [out-chan tiles]
  (loop [tile (read-tile out-chan)]
    (when tile
      (swap! tiles assoc (first tile) (last tile))
      (recur (read-tile out-chan)))))

(defn display [tiles]
  (let [board (into {} tiles)
        max-x (->> board keys (map first) (apply max))
        max-y (->> board keys (map last) (apply max))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (case (get tiles [x y] 0)
          0
          (print " ")
          1
          (print "|")
          2
          (print "B")
          3
          (print "-")
          4
          (print "*")))
      (println))
    (println (get tiles [-1 0] 0))))

(defn position-of [tiles target-tile]
  (-> (filter (fn [[_ tile]] (= tile target-tile)) tiles)
      first
      first))

(defn move-paddle [ball paddle]
  (compare (first ball) (first paddle)))

(defn run []
  (let [tiles (atom {})
        in (fn []
             (display @tiles)
             (println)
             (move-paddle (position-of @tiles 4)
                          (position-of @tiles 3)))
        out-chan (async/chan)
        f (future (intcode/execute-instructions :arcade
                                                memory
                                                in
                                                (intcode/chan->out out-chan)
                                                (intcode/halt-chans out-chan)))]
    (future (read-tiles out-chan tiles))
    @f
    (display @tiles)))
