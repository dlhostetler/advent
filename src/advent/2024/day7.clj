(ns advent.2024.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-line [line]
  (let [[target operands] (str/split line #":")]
    {:operands (map #(Long/parseLong %) (str/split (str/trim operands) #" "))
     :target (Long/parseLong target)}))

(def input
  (->> (-> "resources/2024/day7.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-line)))

(defn valid? [target operands total]
  (if (empty? operands)
    (= total target)
    (or (valid? target
                (rest operands)
                (+ total (first operands)))
        (valid? target
                (rest operands)
                (* total (first operands)))
        (valid? target
                (rest operands)
                (Long/parseLong (str total (first operands)))))))

(defn valid-equation? [{:keys [operands target]}]
  (valid? target (rest operands) (first operands)))

(defn run []
  (->> input
       (filter valid-equation?)
       (map :target)
       (reduce +)))
