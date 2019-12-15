(ns advent.2019.day15
  (:require [advent.2019.grid :as grid]
            [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]))

(def memory
  (intcode/file->memory "resources/day15.input"))

(def dir-ints
  {:east  4
   :north 1
   :south 2
   :west  3})

(defn dir->int [direction]
  (or (get dir-ints direction)
      (throw (ex-info "Invalid direction." {:direction direction}))))

(defn display [{:keys [area droid]}]
  (grid/print (assoc area droid :droid)
              (fn [point]
                (get {:droid   "D"
                      :nothing "."
                      :o2      "O"
                      :origin  "@"
                      :wall    "#"} point))
              {:default     0
               :empty-point " "}))

(defn east [position]
  (update position 0 inc))

(defn north [position]
  (update position 1 inc))

(defn south [position]
  (update position 1 dec))

(defn west [position]
  (update position 0 dec))

(defn next-position [position direction]
  (case direction
    :east
    (east position)
    :north
    (north position)
    :south
    (south position)
    :west
    (west position)
    ;; else
    (throw (ex-info "Invalid direction." {:direction direction}))))

(defn apply-status [{:keys [steps target-point] :as state} status]
  (println "applying status" status)
  (case status
    ;; wall
    0
    (update state :area assoc target-point :wall)
    ;; moved
    1
    (-> state
        (update :area assoc target-point :nothing)
        (assoc :droid target-point))
    ;; moved and found o2 sensor
    2
    (do
      (println "Found O2 system at" target-point "after" steps "steps")
      (display (assoc state :droid target-point))
      (System/exit 1))
    #_(-> state
        (update :area assoc target-point :o2)
        (assoc :droid target-point))))

(defn pick-dir [{:keys [area droid]}]
  (->> [[(north droid) :north]
        [(east droid) :east]
        [(south droid) :south]
        [(west droid) :west]]
       (remove (comp area first))
       first
       last))

(defn random-dir []
  (rand-nth [:north :east :south :west]))

(defn run []
  (let [state (atom {:area  {[0 0] :origin}
                     :droid [0 0]
                     :steps 0})
        out-chan (async/chan 1)
        in (fn []
             (when (:target-point @state)
               (let [status (async/<!! out-chan)]
                 (swap! state apply-status status)))
             #_(println (with-out-str (clojure.pprint/pprint @state)))
             (display @state)
             (let [direction (pick-dir @state)
                   direction (if direction
                               (do
                                 (swap! state update :steps inc)
                                 direction)
                               (do
                                 (swap! state update :steps dec)
                                 (random-dir)))]
               (swap! state assoc :target-point (next-position (:droid @state) direction))
               (println "moving" direction)
               (dir->int direction)))]
    (intcode/execute-instructions :arcade
                                  memory
                                  in
                                  (intcode/chan->out out-chan)
                                  (intcode/halt-chans out-chan))
    nil))
