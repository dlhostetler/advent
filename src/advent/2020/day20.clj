(ns advent.2020.day20
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def tile-size 10)
(def all-txs (combo/cartesian-product [0 90 180 270]
                                      [false true]
                                      [false true]))

(defn parse-tile [lines]
  (let [[header grid] (str/split lines #"\n" 2)
        [tile-id] (rest (re-matches #"Tile (\d+):" header))]
    [(Integer/parseInt tile-id)
     (str/split-lines grid)]))

(defn parse-tiles []
  (->> (-> "resources/2020/day20.input"
           slurp
           (str/split #"\n\n"))
       (map parse-tile)
       (into {})))

(defn negate [n size]
  (- size n 1))

(defn flip-dimension [dimension flip? size]
  (if flip? (negate dimension size) dimension))

;; 90 = (b, -a); 180 = (-a, -b); 270 = (-b, a); 360 = (a, b)
(defn rotate-xy [[x y] degrees size]
  (case degrees
    0
    [x y]
    90
    [y (negate x size)]
    180
    [(negate x size) (negate y size)]
    270
    [(negate y size) x]))

(defn transform-xy [[x y] [degrees flip-x? flip-y?] size]
  (-> [(flip-dimension x flip-x? size)
       (flip-dimension y flip-y? size)]
      (rotate-xy degrees size)))

(defn tile-at [grid xy tx]
  (let [[x y] (transform-xy xy tx tile-size)]
    (-> grid
        (nth y)
        (subs x (inc x)))))

(defn show [grid tx]
  (doseq [y (range tile-size)]
    (doseq [x (range tile-size)]
      (print (tile-at grid [x y] tx)))
    (println)))

(defn show-picture [tiles layout picture-size]
  (doseq [tile-y (range picture-size)]
    (doseq [y (range 1 (dec tile-size))]
      (doseq [tile-x (range picture-size)
              :let [[tile-id tx] (get layout [tile-x tile-y])
                    grid (get tiles tile-id)]]
        (doseq [x (range 1 (dec tile-size))]
          (print (tile-at grid [x y] tx)))
        (print " "))
      (println))
    (println)))

(defn border-e [grid tx]
  (-> (for [y (range tile-size)]
        (tile-at grid [(dec tile-size) y] tx))
      (str/join)))

(defn border-n [grid tx]
  (-> (for [x (range tile-size)]
        (tile-at grid [x 0] tx))
      (str/join)))

(defn border-s [grid tx]
  (-> (for [x (range tile-size)]
        (tile-at grid [x (dec tile-size)] tx))
      (str/join)))

(defn border-w [grid tx]
  (-> (for [y (range tile-size)]
        (tile-at grid [0 y] tx))
      (str/join)))

(defn all-borders-tx [grid tx]
  [(border-e grid tx)
   (border-n grid tx)
   (border-s grid tx)
   (border-w grid tx)])

(defn all-borders [grid]
  (->> (mapcat all-borders-tx (repeat grid) all-txs)
       (into #{})))

(defn matches [tile-borders [tile-id borders]]
  [tile-id (->> (for [[other-tile-id other-borders] tile-borders
                      :when (not= other-tile-id tile-id)
                      :let [common-borders (set/intersection borders other-borders)]
                      :when (pos? (count common-borders))]
                  other-tile-id)
                (into #{}))])

(defn at-corner? [[x y] picture-size]
  (and (or (zero? x)
           (= x (dec picture-size)))
       (or (zero? y)
           (= y (dec picture-size)))))

(defn at-edge? [[x y] picture-size]
  (or (zero? x)
      (= x (dec picture-size))
      (zero? y)
      (= y (dec picture-size))))

(defn corner? [[_ adjacent]]
  (= (count adjacent) 2))

(defn edge? [[_ adjacent]]
  (<= (count adjacent) 3))

(defn adjacent? [tile-ids [_ neighbors]]
  (set/subset? (set tile-ids) neighbors))

(def neighbor-xys
  (memoize
    (fn [xy]
      (let [offsets [[0 1] [0 -1] [1 0] [-1 0]]]
        (->> (map (fn [offset]
                    (mapv + xy offset))
                  offsets)
             set)))))

(defn neighbors [layout xy]
  (let [n (->> (map #(get layout %) (neighbor-xys xy))
               (filter identity)
               set)]
    (when (empty? n)
      (throw (Exception. (str "No neighbors found for " xy))))
    n))

(defn tile-for [adjacent-mapping picture-size layout xy]
  (let [unmapped (reduce dissoc
                         adjacent-mapping
                         (vals layout))]
    (assoc layout
      xy
      (cond
        (= [0 0] xy)
        (-> (filter corner? unmapped)
            (nth 0)
            first)

        (at-corner? xy picture-size)
        (->> unmapped
             (filter corner?)
             (filter #(adjacent? (neighbors layout xy) %))
             first
             first)

        (at-edge? xy picture-size)
        (->> unmapped
             (filter edge?)
             (filter #(adjacent? (neighbors layout xy) %))
             first
             first)

        :else
        (->> unmapped
             (filter #(adjacent? (neighbors layout xy) %))
             first
             first)))))

(defn arrange [neighbor-mapping picture-size]
  (reduce #(tile-for neighbor-mapping picture-size %1 %2)
          {}
          (->> (combo/cartesian-product (range picture-size)
                                        (range picture-size))
               (map vec)
               sort)))


(defn correct-border-e? [tiles layout [x y] tx]
  (let [tile-id (get layout [x y])
        grid (get tiles tile-id)
        border (border-e grid tx)
        layout-info (get layout [(inc x) y])]
    (cond
      ;; already oriented, use its tx
      (and layout-info (vector? layout-info))
      (= border
         (border-w (get tiles (first layout-info))
                   (last layout-info)))

      ;; not oriented, needs to match some
      layout-info
      (some #(= border %) (all-borders (get tiles layout-info)))

      ;; no tile, must be okay (probably a corner)
      :else
      true)))

(defn correct-border-n? [tiles layout [x y] tx]
  (let [tile-id (get layout [x y])
        grid (get tiles tile-id)
        border (border-n grid tx)
        layout-info (get layout [x (dec y)])]
    (cond
      ;; already oriented, use its tx
      (and layout-info (vector? layout-info))
      (= border
         (border-s (get tiles (first layout-info))
                   (last layout-info)))

      ;; not oriented, needs to match some
      layout-info
      (some #(= border %) (all-borders (get tiles layout-info)))

      ;; no tile, must be okay (probably a corner)
      :else
      true)))

(defn correct-border-s? [tiles layout [x y] tx]
  (let [tile-id (get layout [x y])
        grid (get tiles tile-id)
        border (border-s grid tx)
        layout-info (get layout [x (inc y)])]
    (cond
      ;; already oriented, use its tx
      (and layout-info (vector? layout-info))
      (= border
         (border-n (get tiles (first layout-info))
                   (last layout-info)))

      ;; not oriented, needs to match some
      layout-info
      (some #(= border %) (all-borders (get tiles layout-info)))

      ;; no tile, must be okay (probably a corner)
      :else
      true)))

(defn correct-border-w? [tiles layout [x y] tx]
  (let [tile-id (get layout [x y])
        grid (get tiles tile-id)
        border (border-w grid tx)
        layout-info (get layout [(dec x) y])]
    (cond
      ;; already oriented, use its tx
      (and layout-info (vector? layout-info))
      (= border
         (border-e (get tiles (first layout-info))
                   (last layout-info)))

      ;; not oriented, needs to match some
      layout-info
      (some #(= border %) (all-borders (get tiles layout-info)))

      ;; no tile, must be okay (probably a corner)
      :else
      true)))

(defn correct-tx? [tiles layout xy tx]
  (and (correct-border-e? tiles layout xy tx)
       (correct-border-n? tiles layout xy tx)
       (correct-border-s? tiles layout xy tx)
       (correct-border-w? tiles layout xy tx)))

(defn orient [tiles layout xy]
  (let [tx (->> all-txs
                (filter #(correct-tx? tiles layout xy %))
                first)]
    (assoc layout xy [(get layout xy) tx])))

(defn stringify [tiles layout picture-size]
  (let [builder (StringBuilder.)]
    (doseq [tile-y (range picture-size)]
      (doseq [y (range 1 (dec tile-size))]
        (doseq [tile-x (range picture-size)
                :let [[tile-id tx] (get layout [tile-x tile-y])
                      grid (get tiles tile-id)]]
          (doseq [x (range 1 (dec tile-size))]
            (.append builder (tile-at grid [x y] tx))))
        (.append builder "\n")))
    (str/split (str/trim (.toString builder)) #"\n")))

(defn orient-strings [strings tx]
  (->> (for [y (range (count strings))]
         (->> (for [x (range (-> strings first count))
                    :let [[x y] (transform-xy [x y] tx (count strings))]]
                (-> strings
                    (nth y)
                    (subs x (inc x))))
              (apply str)))
       (into [])))

(defn valid-location? [strings x y]
  (and (>= x 0)
       (< x (-> strings first count))
       (>= y 0)
       (< y (count strings))))

(defn mark-if-hash [strings [x y] [offset-x offset-y]]
  (let [x (+ x offset-x)
        y (+ y offset-y)]
    (when (and (valid-location? strings x y)
               (= "#" (-> strings (nth y) (subs x (inc x)))))
      (assoc strings y (apply str (-> (nth strings y)
                                      vec
                                      (assoc x \O)))))))

(defn mark-dragon-head [strings xy]
  (mark-if-hash strings xy [0 0]))

(defn mark-dragon-mid [strings xy]
  (some-> strings
          (mark-if-hash xy [-1 1])
          (mark-if-hash xy [0 1])
          (mark-if-hash xy [1 1])
          (mark-if-hash xy [-6 1])
          (mark-if-hash xy [-7 1])
          (mark-if-hash xy [-12 1])
          (mark-if-hash xy [-13 1])
          (mark-if-hash xy [-18 1])))

(defn mark-dragon-body [strings xy]
  (some-> strings
          (mark-if-hash xy [-2 2])
          (mark-if-hash xy [-5 2])
          (mark-if-hash xy [-8 2])
          (mark-if-hash xy [-11 2])
          (mark-if-hash xy [-14 2])
          (mark-if-hash xy [-17 2])))

(defn mark-dragon [strings xy]
  (if-let [dragonized (some-> strings
                              (mark-dragon-head xy)
                              (mark-dragon-mid xy)
                              (mark-dragon-body xy))]
    dragonized
    strings))

(defn mark-dragons [strings]
  (reduce mark-dragon
          strings
          (->> (combo/cartesian-product (range (-> strings first count))
                                        (range (count strings)))
               (map vec)
               sort)))

(defn dragons? [strings]
  (-> strings
      str/join
      (str/includes? "O")))

(defn run []
  (let [tiles (parse-tiles)
        picture-size (-> tiles count (Math/sqrt) int)
        tile-borders (map-vals all-borders tiles)
        adjacent-mapping (->> (map #(matches tile-borders %) tile-borders)
                              (into {}))
        layout (arrange adjacent-mapping picture-size)
        oriented (reduce #(orient tiles %1 %2) layout (-> layout keys sort))
        strings (stringify tiles oriented picture-size)]
    (->> (map #(orient-strings strings %) all-txs)
         (map mark-dragons)
         (filter dragons?)
         first
         (str/join)
         seq
         (filter (partial = \#))
         count)))
