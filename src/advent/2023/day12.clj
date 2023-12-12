(ns advent.2023.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-row [line]
  (let [[pattern groups] (str/split line #" ")]
    {:groups (->> (str/split groups #",")
                  (mapv #(Integer/parseInt %)))
     :pattern pattern}))

(def rows
  (->> (-> "resources/2023/day12.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-row)))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn valid? [pattern groups]
  (let [counts (->> (str/split pattern #"[^#]+")
                    (map count)
                    (remove zero?))]
    (and (= (count groups) (count counts))
         (->> groups
              (map = counts)
              (every? true?)))))

(defn invalid? [pattern groups]
  (if (str/includes? pattern "?")
    false
    (not (valid? pattern groups))))

(declare ->arrangements)

(defn try-replacement [{pattern :pattern groups :groups :as row} idx char arrangements]
  (if (= (nth pattern idx) \?)
    (let [next-pattern (replace-at pattern idx char)]
      (if (invalid? next-pattern groups)
        arrangements
        (->arrangements (assoc row :pattern next-pattern)
                        (inc idx)
                        arrangements)))
    (->arrangements row (inc idx) arrangements)))

(defn ->arrangements
  ([row]
   (->arrangements row 0 #{}))
  ([{pattern :pattern groups :groups :as row} idx arrangements]
   ;(println idx pattern)
   (if (or (>= idx (count pattern))
           (not (str/includes? pattern "?")))
     (if (valid? pattern groups)
       (conj arrangements pattern)
       arrangements)
     (set/union (try-replacement row idx \# arrangements)
                (try-replacement row idx \. arrangements)))))

(alter-var-root #'->arrangements memoize)

(defn run []
  (->> rows
       (pmap ->arrangements)
       (map count)
       (reduce +)))
