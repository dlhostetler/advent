(ns advent.2019.day23
  (:require [advent.2019.intcode :as intcode])
  (:import (java.util.concurrent ConcurrentLinkedQueue)))

(defn ->queue []
  (ConcurrentLinkedQueue.))

(defn ->nic [address q broadcast]
  (let [memory (intcode/file->memory "resources/2019/day23.input")
        init (atom address)
        next (atom nil)
        input (fn []
                (if-let [address @init]
                  ;; return address first
                  (do
                    (reset! init nil)
                    address)
                  (if-let [y @next]
                    ;; return y if x was sent already
                    (do
                      (reset! next nil)
                      y)
                    (if-let [[x y] (.poll q)]
                      ;; return x if new packet available
                      (do
                        (reset! next y)
                        x)
                      ;; return -1 if nothing waiting
                      -1))))
        out-packet (atom [])
        output (fn [x]
                 (swap! out-packet conj x)
                 (when (= 3 (count @out-packet))
                   (broadcast @out-packet)
                   (reset! out-packet [])))]
    (-> #(intcode/execute-instructions address
                                       memory
                                       input
                                       output
                                       nil)
        (Thread.)
        (.start))))

(defn broadcaster [qs nat-packet]
  (fn [[address x y :as packet]]
    (if (= 255 address)
      (reset! nat-packet [x y])
      (if-let [q (get qs address)]
        (do
          (println (str "Queueing " packet))
          (.add ^ConcurrentLinkedQueue q [x y]))
        (println (str "Dropped packet " packet))))))

(defn idle? [qs]
  (every? #(.isEmpty %) (vals qs)))

(defn poke [qs packet delivered]
  (.add ^ConcurrentLinkedQueue (get qs 0) packet)
  (println "NAT delivered" packet)
  (when (= (-> delivered last last) (last packet))
    (println "DUPLICATE Y: " (last packet)))
  (conj delivered packet))

(defn ->nat [qs nat-packet]
  (loop [delivered []]
    (Thread/sleep 500)
    (if (idle? qs)
      (recur (poke qs @nat-packet delivered))
      (recur delivered))))

(defn run []
  (let [num-nics 50
        nat-packet (atom nil)
        qs (into {} (map vector (range num-nics) (repeatedly ->queue)))
        broadcast (broadcaster qs nat-packet)
        nat (future (->nat qs nat-packet))]
    (doseq [address (range num-nics)]
      (->nic address (get qs address) broadcast))
    @nat))