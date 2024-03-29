(ns advent.2023.day22
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-brick [[idx line]]
  (let [[start end] (sort (str/split line #"~"))
        [startx starty startz] (->> (str/split start #",")
                                    (map #(Integer/parseInt %)))
        [endx endy endz] (->> (str/split end #",")
                              (map #(Integer/parseInt %)))]
    (for [x (range startx (inc endx))
          y (range starty (inc endy))
          z (range startz (inc endz))]
      [[x y z] idx])))

(def snapshot
  (->> (-> "resources/2023/day22.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map-indexed vector)
       (mapcat parse-brick)
       (into {})))

(def max-x (->> snapshot keys (map first) (reduce max)))
(def max-y (->> snapshot keys (map second) (reduce max)))
(def max-z (->> snapshot keys (map last) (reduce max)))

(defn at-z [z]
  (fn [pair]
    (= (-> pair first last) z)))

(defn print-altitude [bricks z]
  (println (str z ":"))
  (-> (->> bricks
           (filter (at-z z))
           (map-keys butlast)
           (map-keys vec)
           (into {}))
      (grid/print (constantly "*") {:max-x max-x
                                    :max-y max-y
                                    :min-x 0
                                    :min-y 0})))

(defn print-altitude-> [bricks z]
  (print-altitude bricks z)
  bricks)

(defn print-altitudes [bricks]
  (doseq [z (reverse (range 0 (inc max-z)))]
    (print-altitude bricks z)))

(defn print-altitudes-> [bricks]
  (print-altitudes bricks)
  bricks)

(defn ->cubes [bricks brick-id]
  (->> bricks
       (filter (comp #(= brick-id %) last))
       (map first)))

(defn bottom-cubes [cubes]
  (let [lowest-z (->> cubes (map last) (reduce min))]
    (->> cubes (filter (comp #(= lowest-z %) last)))))

(defn lowest-z [bricks brick-id]
  (->> (->cubes bricks brick-id)
       (map last)
       (reduce min)))

(defn num-down [bricks down [x y z]]
  (if (= z 1)
    down
    (let [z' (dec z)]
      (if (get bricks [x y z'])
        down
        (recur bricks (inc down) [x y z'])))))

(defn move-cube-z [bricks [x y z :as cube] down]
  (let [brick-id (get bricks cube)]
    (-> bricks
        (dissoc cube)
        (assoc [x y (- z down)] brick-id))))

(defn move-cubes-z [bricks cubes down]
  (reduce #(move-cube-z %1 %2 down) bricks cubes))

(defn fall-brick [bricks brick-id]
  (let [cubes (->cubes bricks brick-id)
        down (->> cubes
                  bottom-cubes
                  (map #(num-down bricks 0 %))
                  (reduce min))]
    (if (zero? down)
      bricks
      (move-cubes-z bricks cubes down))))

(defn ->ordered-brick-ids [bricks]
  (->> bricks
       (sort-by (comp last first))
       reverse
       (map last)
       distinct
       reverse))

(defn fall-bricks
  ([bricks]
   (fall-bricks bricks 0))
  ([bricks i]
   (let [brick-ids (->ordered-brick-ids bricks)
         bricks' (reduce fall-brick bricks brick-ids)]
     (if (= bricks bricks')
       bricks
       (recur bricks' (inc i))))))

(defn without-brick [bricks brick-id]
  (->> bricks
       (remove (comp #(= brick-id %) last))
       (into {})))

(defn brick-ids->z [bricks]
  (for-map [brick-id (->> bricks vals distinct)]
    brick-id (lowest-z bricks brick-id)))

(defn num-fallen [bricks brick-id]
  (println brick-id)
  (let [ids->zs (-> bricks
                    (without-brick brick-id)
                    fall-bricks
                    brick-ids->z)]
    (count (for [[brick-id orig-z] (brick-ids->z bricks)
                 :when (not= (get ids->zs brick-id) orig-z)]
             brick-id))))

(defn run []
  (let [settled (fall-bricks snapshot)]
    (->> settled
         vals
         sort
         distinct
         (pmap #(num-fallen settled %))
         (reduce +))))
