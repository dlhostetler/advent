(ns advent.2021.day14
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def polymer-template-str "NBOKHVHOSVKSSBSVVBCS")

(def polymer-template
  (->> polymer-template-str
       seq
       (mapv str)
       (partition 2 1)
       (map vec)
       (map (fn [pair] [pair 1]))
       (reduce (fn [m [k v]] (update m k (fnil + 0) v)) {})))

(defn split-rule [s]
  (str/split s #" -> "))

(defn parse-pair [pairStr]
  (->> pairStr
       seq
       (mapv str)))

(def rules
  (->> "resources/2021/day14.input"
       io/reader
       line-seq
       (map split-rule)
       (map-keys parse-pair)))

(defn next-elements [polymer [[a b :as pair] count]]
  (let [element (get rules pair)]
    (-> polymer
        (update [a element] (fnil + 0) count)
        (update [element b] (fnil + 0) count))))

(defn next-step [polymer]
  (reduce next-elements {} polymer))

(defn count-element [element-counts [[a _] count]]
  (update element-counts a (fnil + 0) count))

(defn max-element [min-element-pair element-pair]
  (if (> (val element-pair) (val min-element-pair))
    element-pair
    min-element-pair))

(defn min-element [min-element-pair element-pair]
  (if (< (val element-pair) (val min-element-pair))
    element-pair
    min-element-pair))

(defn run []
  (let [polymer (->> polymer-template
                     (seq/successive next-step)
                     (drop 40)
                     first)
        element-counts (reduce count-element
                               {(-> polymer-template-str last str) 1}
                               polymer)
        least-common (reduce min-element element-counts)
        most-common (reduce max-element element-counts)]
    (- (val most-common) (val least-common))))
