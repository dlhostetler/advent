(ns advent.2020.day12
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def init-state {:coordinates [0 0]
                 :heading :e
                 :waypoint [10 1]})

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

(defn degrees-to-times [degrees]
  (-> (Math/abs ^int degrees)
      (/ 90)
      (Math/floor)
      int))

;; dir is 0 (clockwise) or 1 (counter-clockwise)
(defn rotate-waypoint-once [waypoint dir]
  (-> waypoint
      (update dir * -1)
      reverse
      vec))

(defn waypoint-rotations [waypoint dir]
  (seq/successive rotate-waypoint-once waypoint dir))

(defn rotate-waypoint [waypoint degrees]
  (let [dir (if (pos? degrees) 0 1)]
    (->> (waypoint-rotations waypoint dir)
         (drop (degrees-to-times degrees))
         first)))

(defmulti next-state
          (fn [state command]
            (:command command)))

(defmethod next-state :e [state {:keys [value]}]
  (update state :waypoint move [value 0]))

(defmethod next-state :f [state {:keys [value]}]
  (let [distance (map (partial * value)
                      (:waypoint state))]
    (update state :coordinates move distance)))

(defmethod next-state :l [state {:keys [value]}]
  (update state :waypoint rotate-waypoint (* value -1)))

(defmethod next-state :n [state {:keys [value]}]
  (update state :waypoint move [0 value]))

(defmethod next-state :r [state {:keys [value]}]
  (update state :waypoint rotate-waypoint value))

(defmethod next-state :s [state {:keys [value]}]
  (update state :waypoint move [0 (* value -1)]))

(defmethod next-state :w [state {:keys [value]}]
  (update state :waypoint move [(* value -1) 0]))

(defn travel [commands]
  (loop [state init-state
         commands commands]
    (if-not (empty? commands)
      (recur (next-state state (first commands)) (rest commands))
      state)))

(defn distance [state]
  (->> state
       :coordinates
       (map #(Math/abs ^int %))
       (reduce +)))

(defn run []
  (->> (parse-commands)
       travel
       distance))
