(ns advent.2019.grid
  (:require [clojure.string :as str])
  (:refer-clojure :rename {print core-print}))

;; Path
;; ====

(defprotocol Segment
  (describe [this])
  (length [this])
  (move-position [this position])
  (x [this position distance])
  (y [this position distance]))

(defrecord Down [length]
  Segment
  (describe [this] "down")
  (length [this] length)
  (move-position [this position]
    (update position 1 + (* length -1)))
  (x [this position distance]
    (first position))
  (y [this position distance]
    (+ (second position) (* distance -1))))

(defrecord Left [length]
  Segment
  (describe [this] "left")
  (length [this] length)
  (move-position [this position]
    (update position 0 + (* length -1)))
  (x [this position distance]
    (+ (first position) (* distance -1)))
  (y [this position distance]
    (second position)))

(defrecord Right [length]
  Segment
  (describe [this] "right")
  (length [this] length)
  (move-position [this position]
    (update position 0 + length))
  (x [this position distance]
    (+ (first position) distance))
  (y [this position distance]
    (second position)))

(defrecord Up [length]
  Segment
  (describe [this] "up")
  (length [this] length)
  (move-position [this position]
    (update position 1 + length))
  (x [this position distance]
    (first position))
  (y [this position distance]
    (+ (second position) distance)))

(defn ->segment [segment]
  (let [direction (subs segment 0 1)
        length (Integer/parseInt (subs segment 1))]
    (case direction
      "D"
      (->Down length)
      "L"
      (->Left length)
      "R"
      (->Right length)
      "U"
      (->Up length))))

(defn visit [grid x y id steps]
  (println (name id) "is visiting" x y "after" steps)
  (update grid [x y] (fnil conj []) {:id    id
                                     :steps steps}))

(defn visit-points [{:keys [grid id position steps]} segment]
  (reduce (fn [g l]
            (visit g
                   (x segment position l)
                   (y segment position l)
                   id
                   (+ steps l)))
          grid
          (range 1 (inc (length segment)))))

(defn- segment [{:keys [position steps] :as context} segment]
  (println "Mapping segment" (describe segment) (length segment)
           "starting at" position "with" steps "steps")
  (-> context
      (assoc :grid (visit-points context segment))
      (update :position (partial move-position segment))
      (update :steps + (length segment))))

(defn path [grid id path]
  (let [init-context {:grid     grid
                      :id       id
                      :position [0, 0]
                      :steps    0}]
    (->> (str/split path #",")
         (map ->segment)
         (reduce segment init-context)
         :grid)))

;; Visualization
;; =============

(defn- bound [grid dimension-fn agg-fn]
  (->> grid
       keys
       (map dimension-fn)
       (apply agg-fn)))

(defn- grid->max-y [grid]
  (bound grid second max))

(defn- grid->min-y [grid]
  (bound grid second min))

(defn- grid->max-x [grid]
  (bound grid first max))

(defn- grid->min-x [grid]
  (bound grid first min))

(defn print
  ([grid visualize-point]
   (print grid visualize-point {}))
  ([grid visualize-point {:keys [default
                                 empty-point
                                 max-x
                                 max-y
                                 min-x
                                 min-y
                                 padding
                                 y-dir]
                          :or   {empty-point "."
                                 padding 1}}]
   (doseq [y (cond-> (range (- (or min-y (grid->min-y grid)) padding)
                            (inc (+ (or max-y (grid->max-y grid)) padding)))
               (not= :top-down y-dir)
               reverse)]
     (doseq [x (range (- (or min-x (grid->min-x grid)) padding)
                      (inc (+ (or max-x (grid->max-x grid)) padding)))
             :let [point (get grid [x y] default)]]
       (core-print (if-let [out (when point (visualize-point point))]
                     out
                     empty-point)))
     (println))))

;; Path
;; ----

(defn visualize-path [point]
  (let [ids (into #{} (map :id point))]
    (cond
      (> (count ids) 1)
      "X"
      (= ids #{:wire1})
      "@"
      (= ids #{:wire2})
      "#")))
