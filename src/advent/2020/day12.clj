(ns advent.2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def init-state {:coordinates [0 0]
                 :heading :e})

(def cardinals [:e :s :w :n])

(defn parse-direction [line]
  (let [[command value] (rest (re-matches #"([A-Z])(.+)" line))]
    {:command (-> command str/lower-case keyword)
     :value (Integer/parseInt value)}))

(defn parse-commands []
  (->> "resources/2020/day12.input"
       io/reader
       line-seq
       (map parse-direction)))

(defn move [coordinates offset]
  [(+ (first coordinates) (first offset))
   (+ (second coordinates) (second offset))])

(defn next-heading [heading degrees]
  (let [current-index (.indexOf cardinals heading)
        next-index (mod (-> degrees
                            (/ 90)
                            (Math/floor)
                            int
                            (+ current-index))
                        (count cardinals))]
    (nth cardinals next-index)))

(defmulti next-state
          (fn [state command]
            (:command command)))

(defmethod next-state :e [state {:keys [value]}]
  (update state :coordinates move [value 0]))

(defmethod next-state :f [state command]
  (next-state state
              {:command (:heading state)
               :value (:value command)}))

(defmethod next-state :l [state {:keys [value]}]
  (update state :heading next-heading (* value -1)))

(defmethod next-state :n [state {:keys [value]}]
  (update state :coordinates move [0 value]))

(defmethod next-state :r [state {:keys [value]}]
  (update state :heading next-heading value))

(defmethod next-state :s [state {:keys [value]}]
  (update state :coordinates move [0 (* value -1)]))

(defmethod next-state :w [state {:keys [value]}]
  (update state :coordinates move [(* value -1) 0]))

(defn travel [commands]
  (loop [state init-state
         commands commands]
    (if-not (empty? commands)
      (recur (next-state state (first commands)) (rest commands))
      state)))

(defn distance [state]
  (->> state
       :coordinates
       (map #(Math/abs %))
       (reduce +)))

(defn run []
  (->> (parse-commands)
       travel
       distance))
