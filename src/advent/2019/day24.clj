(ns advent.2019.day24
  (:require [advent.2019.grid :as grid]
            [clojure.java.io :as io]))

(def width 5)
(def height 5)

(defn parse-bug [x y c]
  (when (= \# c)
    [x y 0]))

(defn parse-row [y row]
  (map-indexed #(parse-bug %1 y %2) row))

(defn parse-area []
  (->> "resources/day24.input"
       io/reader
       line-seq
       (map seq)
       (map-indexed parse-row)
       (mapcat identity)
       (remove nil?)
       (into #{})))

(defn display [bugs]
  (let [grid (into {} (zipmap (mapv butlast bugs) (repeat "#")))]
    (grid/print grid
                (fn [tile]
                  (when tile tile))
                {:default "."
                 :empty-point "."
                 :max-x (dec width)
                 :max-y (dec height)
                 :min-x 0
                 :min-y 0
                 :padding 0
                 :y-dir :top-down}))
  (println)
  bugs)

(defn display-all [bugs]
  (doseq [level (->> bugs
                     (map last)
                     (into #{})
                     sort)]
    (println (str "Level " level ":"))
    (display (filter #(= level (last %)) bugs)))
  (println "-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n")
  bugs)

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 dec))

(defn south [position]
  (update position 1 inc))

(defn west [position]
  (update position 0 dec))

(def center [2 2])
(def center-w (west center))
(def center-e (east center))
(def center-n (north center))
(def center-s (south center))

(defn adjust-outer-edge [[x y level :as neighbor]]
  (cond
    ;; left of area
    (neg? x)
    (conj center-w (dec level))
    ;; right of area
    (>= x width)
    (conj center-e (dec level))
    ;; top of area
    (neg? y)
    (conj center-n (dec level))
    ;; bottom of area
    (>= y height)
    (conj center-s (dec level))
    ;; in area
    :else
    neighbor))

(defn adjust-inner-edge
  [[point-x point-y level :as point] [neighbor-x neighbor-y :as neighbor]]
  (if (= [neighbor-x neighbor-y] center)
    (cond
      ;; center left
      (= center-w [point-x point-y])
      (into [] (for [neighbor-y (range height)]
                 [0 neighbor-y (inc level)]))
      ;; center right
      (= center-e [point-x point-y])
      (into [] (for [neighbor-y (range height)]
                 [(dec width) neighbor-y (inc level)]))
      ;; center top
      (= center-n [point-x point-y])
      (into [] (for [neighbor-x (range width)]
                 [neighbor-x 0 (inc level)]))
      ;; center bottom
      (= center-s [point-x point-y])
      (into [] (for [neighbor-x (range width)]
                 [neighbor-x (dec height) (inc level)]))
      :else
      (throw (ex-info "Unknown neighbor to center." {:neighbor neighbor
                                                     :point point})))
    [neighbor]))

(defn point->neighbors [point]
  (->> [(east point)
        (north point)
        (south point)
        (west point)]
       (map adjust-outer-edge)
       (mapcat #(adjust-inner-edge point %))))

(alter-var-root #'point->neighbors memoize)

(defn bugs->points [bugs]
  (into bugs (mapcat point->neighbors bugs)))

(defn life [bugs next-bugs point]
  (let [bug? (bugs point)
        neighbor-bugs (->> point
                           point->neighbors
                           (filter bugs))
        num-neighbor-bugs (count neighbor-bugs)]
    (cond
      (and bug? (= 1 num-neighbor-bugs))
      (conj next-bugs point)
      (and (not bug?)
           (or (= 1 num-neighbor-bugs)
               (= 2 num-neighbor-bugs)))
      (conj next-bugs point)
      :else
      next-bugs)))

(defn minute [bugs]
  (reduce #(life bugs %1 %2) #{} (bugs->points bugs)))

(defn run []
  (->> (loop [bugs (parse-area)
              n 200]
         (if (pos? n)
           (do
             (display-all bugs)
             (recur (minute bugs) (dec n)))
           bugs))
       display-all
       count))