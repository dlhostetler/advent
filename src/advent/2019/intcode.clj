(ns advent.2019.intcode
  (:require [clojure.core.async :as async :refer [<!! >!!]]))

(def instruction->keyword
  {1 :add
   2 :multiply
   3 :input
   4 :output
   5 :jump-if-true
   6 :jump-if-false
   7 :less-than
   8 :equals
   99 :halt})

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn pad [v length pad-val]
  (let [n (- length (count v))]
    (if (pos? n)
      ;; need to extend the array
      (into v (repeat n pad-val))
      v)))

(defn- current-instruction [{:keys [memory pointer]}]
  (-> (nth memory pointer)
      (mod 100)
      instruction->keyword))

(defn- advance-pointer [state args]
  (update state :pointer + (inc (count args))))

(defn- set-pointer [state pointer]
  (assoc state :pointer pointer))

(defn- halt-pointer [state]
  (set-pointer state -1))

(defn- deref-param [memory i]
  (let [memory-index (nth memory i)]
    (nth memory memory-index)))

(defn safe-assoc [v i val]
  (assoc (pad v i 0) i val))

(defn- parse-args [{:keys [memory pointer]} args]
  (let [position-modes (-> (nth memory pointer)
                           (/ 100)
                           int
                           digits
                           reverse
                           vec
                           (pad (count args) 0))]
    (into [] (for [i (range (count args))
                   :let [arg-type (nth args i)
                         offset (+ pointer i 1)
                         position-mode (nth position-modes i)]]

               (cond
                 (and (= :in arg-type) (= 0 position-mode))
                 (deref-param memory offset)
                 (or (= :out arg-type) (= 1 position-mode))
                 (nth memory offset)
                 :else
                 (throw (ex-info "Unknown position mode."
                                 {:position-mode position-mode})))))))

(defn- log-prefix [{:keys [id pointer]}]
  (str "[" (name id) ":" pointer "]"))

(defn- log [state & more]
  (apply println
         (log-prefix state)
         more))

(defmulti execute-instruction current-instruction)

(defmethod execute-instruction :add [state]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)
        result (+ arg0 arg1)]
    (log state arg0 "+" arg1 "=" (str result "@" out-pos))
    (-> state
        (update :memory safe-assoc out-pos result)
        (advance-pointer args))))

(defmethod execute-instruction :equals [state]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)
        result (if (= arg0 arg1) 1 0)]
    (log state arg0 "==" arg1 "=" (str result "@" out-pos))
    (-> state
        (update :memory safe-assoc out-pos result)
        (advance-pointer args))))

(defmethod execute-instruction :halt [state]
  (log state "halt")
  (halt-pointer state))

(defn- chan-input [{:keys [in-chan]}]
  (when in-chan
    (<!! in-chan)))

(defn- stdin-input [state]
  (print (log-prefix state) "input> ")
  (flush)
  (Integer/parseInt (read-line)))

(defmethod execute-instruction :input [state]
  (let [args [:out]
        [out-pos] (parse-args state args)]
    (let [i (or (chan-input state)
                (stdin-input state))]
      (log state (str i "->" out-pos))
      (-> state
          (update :memory safe-assoc out-pos i)
          (advance-pointer args)))))

(defmethod execute-instruction :jump-if-false [state]
  (let [args [:in :in]
        [arg0 arg1] (parse-args state args)]
    (if (zero? arg0)
      (do
        (log state "jumping to" arg1)
        (set-pointer state arg1))
      (do
        (log state "ignoring jump to" arg1)
        (advance-pointer state args)))))

(defmethod execute-instruction :jump-if-true [state]
  (let [args [:in :in]
        [arg0 arg1] (parse-args state args)]
    (if-not (zero? arg0)
      (do
        (log state "jumping to" arg1)
        (set-pointer state arg1))
      (do
        (log state "ignoring jump to" arg1)
        (advance-pointer state args)))))

(defmethod execute-instruction :less-than [{:keys [pointer] :as state}]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)
        result (if (< arg0 arg1) 1 0)]
    (log state arg0 "<" arg1 "=" (str result "@" out-pos))
    (-> state
        (update :memory safe-assoc out-pos result)
        (advance-pointer args))))

(defmethod execute-instruction :multiply [state]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)
        result (* arg0 arg1)]
    (log state arg0 "*" arg1 "=" (str result "@" out-pos))
    (-> state
        (update :memory safe-assoc out-pos result)
        (advance-pointer args))))

(defmethod execute-instruction :output [{:keys [memory out-chan pointer] :as state}]
  (let [args [:out]
        [arg0] (parse-args state args)
        val (get memory arg0)]
    (log state (str val "@" arg0) "out")
    (if out-chan
      (>!! out-chan val)
      (println val))
    (advance-pointer state args)))

;; Public
;; ======

(defn execute-instructions
  ([id memory]
   (execute-instructions id memory nil nil))
  ([id memory in-chan out-chan]
   (loop [next {:id id
                :in-chan in-chan
                :memory memory
                :out-chan out-chan
                :pointer 0}]
     (if-not (-> next :pointer neg?)
       (recur (execute-instruction next))
       (:memory next)))))
