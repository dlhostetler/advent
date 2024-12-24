(ns advent.2024.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2024/day24.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(def init-wires
  (->> (str/split (first input) #"\n")
       (map (fn [line] (str/split line #": ")))
       (map-vals #(Integer/parseInt %))))

(defn parse-gate [line]
  (let [[_ in0 op in1 out] (re-matches #"(\w+) (\w+) (\w+) -> (\w+)" line)]
    [(-> op str/lower-case keyword) in0 in1 out]))

(def gates
  (->> (str/split (second input) #"\n")
       (map parse-gate)
       (into #{})))

(def all-zs
  (->> gates
       (map last)
       (filter #(str/starts-with? % "z"))
       sort
       reverse))

(defn gate-activated? [wires [_ _ _ out]]
  (contains? wires out))

(defn apply-gate [wires [op in0 in1 out]]
  (let [in0-val (get wires in0)
        in1-val (get wires in1)]
    (if (and in0-val in1-val)
      (assoc wires
             out
             (case op
               :and
               (bit-and in0-val in1-val)
               :or
               (bit-or in0-val in1-val)
               :xor
               (bit-xor in0-val in1-val)))
      ;; can't apply without input
      wires)))

(defn apply-gates [init-state]
  (loop [state init-state
         gates (:gates init-state)]
    (if-let [gate (first gates)]
      (let [next-wires (apply-gate (:wires state) gate)]
        (if (gate-activated? next-wires gate)
          (recur (-> state
                     (assoc :wires next-wires)
                     (update :gates disj gate))
                 (rest gates))
          (recur (assoc state :wires next-wires)
                 (rest gates))))
      state)))

(defn all-zs-set? [{:keys [wires]}]
  (every? #(contains? wires %) all-zs))

(defn wait-for-zs [state]
  (let [next-state (apply-gates state)]
    (cond
      (all-zs-set? state)
      (:wires state)

      (= (:gates state) (:gates next-state))
      (throw (Exception. "no gates were set"))

      :else
      (recur next-state))))

(defn output [wires]
  (->> all-zs
       (map wires)
       (apply str)
       (#(Long/parseLong % 2))))

(defn run []
  (-> {:gates gates :wires init-wires}
      wait-for-zs
      output))
