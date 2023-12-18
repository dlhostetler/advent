(ns advent.2023.day18
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def dir->neighbor {:u grid/north
                    :r grid/east
                    :d grid/south
                    :l grid/west})

(defn parse-instruction [line]
  (let [[_ dirStr amt color] (re-matches #"(.) (\d+) \((.+)\)" line)
        dir (-> dirStr str str/lower-case keyword)]
    {:amt (Integer/parseInt amt)
     :dir dir
     :color color
     :neighbor (dir->neighbor dir)}))

(def instructions
  (->> (-> "resources/2023/day18.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-instruction)))

(defn dig [{:keys [at] :as state} {:keys [amt neighbor]}]
  (let [next-ps (->> (seq/successive neighbor at)
                     (drop 1)
                     (take amt))
        holes (map vector next-ps (repeat \#))]
    (-> state
        (update :ground into holes)
        (assoc :at (last next-ps)))))

(defn run []
  (-> (->> instructions
           (reduce dig {:at [0 0]
                        :ground {[0 0] \@}})
           :ground)
      grid/print->
      (grid/flood {:neighbors-fn grid/eight-neighbors
                   :start-point [115 210]})
      :grid
      grid/print->
      count))
