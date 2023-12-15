(ns advent.2023.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [flatland.ordered.map :refer [ordered-map]]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2023/day15.input"
      io/reader
      slurp
      (str/split #",")))

(defn hash-char [h s]
  (-> h
      (+ (int s))
      (* 17)
      (mod 256)))

(defn hash-string [s]
  (reduce hash-char 0 s))

(defn parse-step [s]
  (let [[_ label op focal-length] (re-matches #"([a-z]+)(-|=)(\d+)?" s)]
    (-> {:label label
         :lens (hash-string label)
         :op op}
        (cond-> focal-length
                (assoc :focal-length (Integer/parseInt focal-length))))))

(defmulti do-step (fn [boxes step] (:op step)))

(defmethod do-step "-" [boxes {:keys [label lens]}]
  (dissoc-in boxes [lens label]))

(defmethod do-step "=" [boxes {:keys [focal-length label lens]}]
  (update boxes
          lens
          (fnil assoc (ordered-map))
          label
          focal-length))

(defn ->lens-power [box-num [idx [_ focal-length]]]
  (* (inc box-num) (inc idx) focal-length))

(defn ->box-power [[box-num lenses]]
  (->> lenses
       (map-indexed vector)
       (map (partial ->lens-power box-num))
       (reduce +)))

(defn run []
  (->> input
       (map parse-step)
       (reduce do-step {})
       (map ->box-power)
       (reduce +)))
