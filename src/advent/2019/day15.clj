(ns advent.2019.day15
  (:require [advent.2019.grid :as grid]
            [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]
            [clojure.edn :as edn]))

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

(defn display-area [area]
  (grid/print area
              (fn [point]
                (get {:droid   "D"
                      :nothing "."
                      :o2      "O"
                      :origin  "@"
                      :wall    "#"} point))
              {:default     0
               :empty-point " "}))

(defn display [{:keys [area droid]}]
  (display-area (assoc area droid :droid)))

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

(defn unexplored? [position state]
  (not (get-in state [:area position])))

(defn at-point [{:keys [droid target-point] :as state}]
  (let [e (east droid)
        n (north droid)
        s (south droid)
        w (west droid)]
    (-> state
        (update :unexplored disj target-point)
        (cond-> (unexplored? e state) (update :unexplored conj e))
        (cond-> (unexplored? n state) (update :unexplored conj n))
        (cond-> (unexplored? s state) (update :unexplored conj s))
        (cond-> (unexplored? w state) (update :unexplored conj w)))))

(defn apply-status [{:keys [target-point] :as state} status]
  #_(println "applying status" status)
  (case status
    ;; wall
    0
    (-> state
        (update :area assoc target-point :wall)
        at-point)
    ;; moved
    1
    (-> state
        (update :area assoc target-point :nothing)
        (assoc :droid target-point)
        at-point)
    ;; moved and found o2 sensor
    2
    (do
      (println "Found O2 system at" target-point)
      (-> state
          (update :area assoc target-point :o2)
          (assoc :droid target-point)
          at-point))))

(defn pick-dir [{:keys [area droid]}]
  (->> [[(north droid) :north]
        [(east droid) :east]
        [(south droid) :south]
        [(west droid) :west]]
       (remove (comp area first))
       first
       last))

(defn random-dir [{:keys [area droid]}]
  (->> [[(north droid) :north]
        [(east droid) :east]
        [(south droid) :south]
        [(west droid) :west]]
       (remove (comp #(= :wall %) area first))
       rand-nth
       last))

(defn run []
  (let [state (atom {:area       {[0 0] :origin}
                     :droid      [0 0]
                     :unexplored #{}})
        out-chan (async/chan 1)
        in (fn []
             (when (:target-point @state)
               (let [status (async/<!! out-chan)]
                 (swap! state apply-status status)))
             #_(println (with-out-str (clojure.pprint/pprint @state)))
             #_(display @state)
             (let [direction (or (pick-dir @state)
                                 (when-not (empty? (:unexplored @state)) (random-dir @state)))]
               (when-not direction
                 (println "not sure where to go")
                 (display @state)
                 (println "\n\n")
                 (clojure.pprint/pprint (:area @state))
                 (System/exit 1))
               (swap! state assoc :target-point (next-position (:droid @state) direction))
               #_(println "moving" direction)
               (dir->int direction)))]
    (intcode/execute-instructions :arcade
                                  memory
                                  in
                                  (intcode/chan->out out-chan)
                                  (intcode/halt-chans out-chan))
    nil))

;; Part 2
;; ======

(def init-area
  (edn/read-string (slurp "resources/day15.area")))

(defn oxygen? [[point thing]]
  (= :o2 thing))

(defn nothing? [[point thing]]
  (= :nothing thing))

(defn set-oxygen [area point]
  (assoc area point :o2))

(defn waft-oxygen [area point]
  (->> [(north point)
        (east point)
        (south point)
        (west point)]
       (filter (comp #(= :nothing %) area))
       (reduce set-oxygen area)))

(defn waft [area]
  (let [o2-points (->> area
                       (filter oxygen?)
                       (mapv first))]
    (reduce waft-oxygen area o2-points)))

(defn run2 []
  (loop [area init-area
         n 0]
    (if (not (empty? (filter nothing? area)))
      (do
        (display-area area)
        (Thread/sleep 1000)
        (recur (waft area) (inc n)))
      (println "It took" n "minutes"))))