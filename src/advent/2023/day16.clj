(ns advent.2023.day16
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def contraption
  (-> "resources/2023/day16.input"
      io/reader
      grid/slurp))

(def dir->dirs {:n {\| #{:n}
                    \- #{:w :e}
                    \\ #{:w}
                    \/ #{:e}
                    \. #{:n}}
                :e {\| #{:n :s}
                    \- #{:e}
                    \\ #{:s},
                    \/ #{:n},
                    \. #{:e}}
                :s {\| #{:s},
                    \- #{:w :e},
                    \\ #{:e},
                    \/ #{:w},
                    \. #{:s}}
                :w {\| #{:n :s}
                    \- #{:w}
                    \\ #{:n}
                    \/ #{:s}
                    \. #{:w}}})
(def dir->neighbor {:n grid/north
                    :e grid/east
                    :s grid/south
                    :w grid/west})

(defn valid-point? [point]
  (grid/valid-point-or-nil contraption point))

(defn next-beams [energized [point dir]]
  (for [next-dir (get-in dir->dirs [dir (get contraption point)])
        :let [to-point ((dir->neighbor next-dir) point)]
        :when (valid-point? to-point)
        :let [next-beam [to-point next-dir]]
        :when (not (@energized next-beam))]
    next-beam))

(defn energize [beam]
  (println beam)
  (let [energized (atom #{})]
    (loop [beams [beam]]
      (swap-vals! energized #(apply conj %1 %2) beams)
      (let [next-beams (mapcat #(next-beams energized %) beams)]
        (when-not (empty? next-beams)
          (recur next-beams))))
    (->> @energized (map first) set count)))

(defn run []
  (->> (concat (for [x (range 0 (grid/max-x+1 contraption))]
                 [[x 0] :s])
               (for [x (range 0 (grid/max-x+1 contraption))]
                 [[x (grid/max-y contraption)] :n])
               (for [y (range 0 (grid/max-y+1 contraption))]
                 [[0 y] :e])
               (for [y (range 0 (grid/max-y+1 contraption))]
                 [[(grid/max-x contraption) y] :w]))
       (map energize)
       (reduce max)))
