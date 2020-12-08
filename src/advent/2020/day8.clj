(ns advent.2020.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-instruction [i line]
  (let [[op arg] (str/split line #" ")]
    {:line i
     :op (keyword op)
     :arg (Integer/parseInt arg)}))

(defn read-instructions []
  (->> "resources/2020/day8.input"
       io/reader
       line-seq
       (map-indexed parse-instruction)
       (into [])))

(defn new-state []
  {:accumulator 0
   :offset 0
   :seen #{}})

(defn seen? [seen offset]
  (contains? seen offset))

(defmulti execute-instruction (fn [state instruction] (:op instruction)))

(defmethod execute-instruction :acc [state {:keys [arg line]}]
  (println (str "[" line "]") "Executing acc" arg)
  (-> state
      (update :accumulator + arg)
      (update :offset + 1)
      (update :seen conj line)))

(defmethod execute-instruction :jmp [state {:keys [arg line]}]
  (println (str "[" line "]") "Executing jmp" arg)
  (-> state
      (update :offset + arg)
      (update :seen conj line)))

(defmethod execute-instruction :nop [state {:keys [line]}]
  (println (str "[" line "]") "Executing nop")
  (-> state
      (update :offset + 1)
      (update :seen conj line)))

(defn execute [instructions]
  (loop [{:keys [offset seen] :as state} (new-state)]
    (cond
      (>= offset (count instructions))
      (do
        (println "End.")
        state)
      (seen? seen offset)
      (do
        (println "Already seen" offset ", stopping.")
        state)
      :else
      (recur (execute-instruction state
                                  (nth instructions offset))))))

(defn flip [instruction]
  (case (:op instruction)
    :jmp (assoc instruction :op :nop)
    :nop (assoc instruction :op :jmp)
    ;; default (no change)
    instruction))

(defn success? [instructions {:keys [offset]}]
  (= offset (count instructions)))

(defn run []
  (let [instructions (read-instructions)]
    (loop [i 0]
      (println "==== Trying" i "====")
      (let [state (execute (update instructions i flip))]
        (if (success? instructions state)
          state
          (recur (inc i)))))))
