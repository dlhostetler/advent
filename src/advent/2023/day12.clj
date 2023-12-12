(ns advent.2023.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def unfold 5)

(defn parse-row [line]
  (let [[pattern groups] (str/split line #" ")]
    {:groups (->> (str/split groups #",")
                  (mapv #(Integer/parseInt %))
                  (repeat unfold)
                  flatten)
     :pattern (str (->> pattern
                        (repeat unfold)
                        (str/join "?"))
                   ".")}))

(def rows
  (->> (-> "resources/2023/day12.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-row)))

(defn safe-nth [coll index]
  (when (< index (count coll))
    (nth coll index)))

(declare count-arrangements)

(defn count-group [{pattern :pattern groups :groups :as row}]
  (let [num-springs (first groups)]
    (cond
      (->> pattern (take num-springs) (some (partial = \.)))
      0

      (= (safe-nth pattern num-springs) \#)
      0

      :else
      (count-arrangements (-> row
                              (update :pattern subs (+ num-springs 1))
                              (update :groups rest))))))

(defn count-arrangements [{pattern :pattern groups :groups :as row}]
  (cond
    (and (zero? (count pattern))
         (zero? (count groups)))
    1

    (zero? (count pattern))
    0

    (and (zero? (count groups))
         (some (partial = \#) pattern))
    0

    (zero? (count groups))
    1

    (< (count pattern)
       (+ (reduce + groups) (dec (count groups))))
    0

    (= (first pattern) \.)
    (count-arrangements (update row :pattern subs 1))

    (= (first pattern) \#)
    (count-group row)

    :else
    (+ (count-arrangements (-> row
                               (update :pattern subs 1)
                               (update :pattern #(str "#" %))))
       (count-arrangements (-> row
                               (update :pattern subs 1)
                               (update :pattern #(str "." %)))))))

(alter-var-root #'count-arrangements memoize)

(defn run []
  (->> rows
       (pmap count-arrangements)
       (reduce +)))
