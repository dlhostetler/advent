(ns advent.2023.day20
  (:require [advent.math :refer [lcm]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def rx-conjunction :zg)

(defn parse-module [line]
  (let [[_ module-type module-name outputs] (re-matches #"(%|&)?(.+) -> (.+)" line)]
    [(keyword module-name)
     {:module-type (cond
                     (= module-name "broadcaster")
                     :broadcaster

                     (= module-type "%")
                     :flip-flop

                     (= module-type "&")
                     :conjunction

                     :else
                     (throw (Exception. (str "unknown type: " line))))
      :outputs (->> (str/split outputs #"\s*,.\s*")
                    (mapv keyword))}]))

(defn reverse-conjunction [modules module-name output]
  (if (= (get-in modules [output :module-type]) :conjunction)
    (assoc-in modules [output :state module-name] :low)
    modules))

(defn reverse-conjunctions [modules module-name outputs]
  (reduce #(reverse-conjunction %1 module-name %2) modules outputs))

(defn initialize-module-state [modules [module-name module]]
  (-> modules
      (cond-> (= (:module-type module) :flip-flop)
              (assoc-in [module-name :state] false))
      (reverse-conjunctions module-name (:outputs module))))

(defn initialize-modules-state [modules]
  (reduce initialize-module-state modules modules))

(def init-modules
  (->> (-> "resources/2023/day20.input"
           io/reader
           slurp
           (str/split #"\n"))
       (map parse-module)
       (into {})
       initialize-modules-state))

(def init-state {:modules init-modules
                 :queued []})

(defn pulse->val [modules pulse field]
  (-> modules
      (get (second pulse))
      (get field)))

(defn pulse->module-type [modules pulse]
  (pulse->val modules pulse :module-type))

(defn pulse->outputs [modules pulse]
  (pulse->val modules pulse :outputs))

(defn pulse->state [modules pulse]
  (pulse->val modules pulse :state))

(defn set-module-state [state module-name module-state]
  (assoc-in state [:modules module-name :state] module-state))

(defn queue-pulses [state from tos pulse-type]
  #_(println "q:" from "->" pulse-type "->" tos)
  (update state
          :queued
          into
          (for [to tos]
            [from to pulse-type])))

(defn queue-pulse [state from to pulse-type]
  (queue-pulses state from [to] pulse-type))

(defmulti process-pulse (fn [state pulse]
                          (pulse->module-type (:modules state) pulse)))

(defmethod process-pulse nil [state pulse]
  state)

(defmethod process-pulse :broadcaster [{:keys [modules] :as state}
                                       [_ module-name pulse-type :as pulse]]
  (reduce #(queue-pulse %1 module-name %2 pulse-type)
          state
          (pulse->outputs modules pulse)))

(defmethod process-pulse :conjunction [{:keys [modules] :as state}
                                       [from module-name pulse-type :as pulse]]
  (let [last-pulses (assoc (pulse->state modules pulse) from pulse-type)
        all-high? (->> last-pulses vals (every? #(= % :high)))]
    (-> state
        (queue-pulses module-name
                      (pulse->outputs modules pulse)
                      (if all-high? :low :high))
        (set-module-state module-name last-pulses))))

(defmethod process-pulse :flip-flop [{:keys [modules] :as state}
                                     [_ module-name pulse-type :as pulse]]
  (if (= pulse-type :low)
    (let [last-on? (pulse->state modules pulse)]
      (-> state
          (queue-pulses module-name
                        (pulse->outputs modules pulse)
                        (if last-on? :low :high))
          (set-module-state module-name (not last-on?))))
    state))

(defn press-button [state]
  #_(println "BUTTON")
  (queue-pulse state :button :broadcaster :low))

(defn process-next [state from-module-name]
  (let [pulse (-> state :queued first)
        delivered? (= (get-in state [:modules rx-conjunction :state from-module-name])
                      :high)]
    #_(println pulse)
    (-> state
        (process-pulse pulse)
        (update :queued rest)
        (update :queued vec)
        (cond-> delivered? (assoc :delivered? true)))))

(defn process-all [state from-module-name]
  (if-not (empty? (:queued state))
    (recur (process-next state from-module-name) from-module-name)
    state))

(defn push-and-process [state n from-module-name]
  (if (:delivered? state)
    n
    (recur (-> state press-button (process-all from-module-name))
           (inc n)
           from-module-name)))

(defn run []
  (->> (get-in init-modules [rx-conjunction :state])
       keys
       (pmap #(push-and-process init-state 0 %))
       (reduce lcm)))
