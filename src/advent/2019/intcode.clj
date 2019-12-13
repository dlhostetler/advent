(ns advent.2019.intcode
  (:require [clojure.core.async :as async :refer [<!! >!!]]))

(def ^:dynamic *log?* false)

(def instruction->keyword
  {1 :add
   2 :multiply
   3 :input
   4 :output
   5 :jump-if-true
   6 :jump-if-false
   7 :less-than
   8 :equals
   9 :relative-base-offset
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

(defn safe-nth [v i]
  (if (>= i (count v))
    0
    (nth v i)))

(defn safe-assoc [v i val]
  (assoc (pad v i 0) i val))

(defn- deref-param [memory i relative-base]
  (let [memory-index (safe-nth memory i)]
    (safe-nth memory (+ memory-index relative-base))))

(defn- parse-in-arg [{:keys [memory relative-base]} arg-offset position-mode]
  (case position-mode
    ;; position mode
    0
    (deref-param memory arg-offset 0)
    ;; immediate mode
    1
    (safe-nth memory arg-offset)
    ;; relative position mode
    2
    (deref-param memory arg-offset relative-base)
    ;; default
    (throw (ex-info (str "Unknown ::in position mode (" position-mode ").")
                    {:position-mode position-mode}))))

(defn- parse-out-arg [{:keys [memory relative-base]} arg-offset position-mode]
  (case position-mode
    ;; position mode
    0
    (safe-nth memory arg-offset)
    ;; relative position mode
    2
    (+ (safe-nth memory arg-offset) relative-base)
    ;; default
    (throw (ex-info (str "Unknown :out position mode (" position-mode ").")
                    {:position-mode position-mode}))))

(defn- parse-args [{:keys [memory pointer] :as state} args]
  (let [position-modes (-> (nth memory pointer)
                           (/ 100)
                           int
                           digits
                           reverse
                           vec
                           (pad (count args) 0))]
    (into [] (for [i (range (count args))
                   :let [arg-type (nth args i)
                         arg-offset (+ pointer i 1)
                         position-mode (nth position-modes i)]]
               (case arg-type
                 :in
                 (parse-in-arg state arg-offset position-mode)
                 :out
                 (parse-out-arg state arg-offset position-mode)
                 (throw (ex-info (str "Unknown arg type (" arg-type ").")
                                 {:arg-type arg-type})))))))

(defn- log-prefix [{:keys [id pointer]}]
  (str "[" (name id) ":" pointer "]"))

(defn- log [state & more]
  (when *log?*
    (apply println
           (log-prefix state)
           more)))

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

(defn- fn-input [{:keys [in-fn]}]
  (when in-fn
    (in-fn)))

(defn- stdin-input [state]
  (print (log-prefix state) "input> ")
  (flush)
  (Integer/parseInt (read-line)))

(defmethod execute-instruction :input [state]
  (let [args [:out]
        [out-pos] (parse-args state args)]
    (let [i (or (fn-input state)
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

(defmethod execute-instruction :less-than [state]
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

(defmethod execute-instruction :output [{:keys [out-chan] :as state}]
  (let [args [:in]
        [arg0] (parse-args state args)]
    (log state arg0 "out")
    (if out-chan
      (>!! out-chan arg0)
      (println arg0))
    (advance-pointer state args)))

(defmethod execute-instruction :relative-base-offset [{:keys [relative-base] :as state}]
  (let [args [:in]
        [arg0] (parse-args state args)
        result (+ relative-base arg0)]
    (log state "relative base" relative-base "+" arg0 "=" result)
    (-> state
        (assoc :relative-base result)
        (advance-pointer args))))

;; Public
;; ======

(defmacro with-logging
  [& body]
  `(binding [*log?* true]
      ~@body))

;; TODO: support channels or functions both (maybe just have a function wrap
;; a channel?)

(defn execute-instructions
  ([id memory]
   (execute-instructions id memory nil nil))
  ([id memory in-fn out-chan]
   (loop [next {:id id
                :in-fn in-fn
                :memory memory
                :out-chan out-chan
                :pointer 0
                :relative-base 0}]
     (if-not (-> next :pointer neg?)
       (recur (execute-instruction next))
       (do
         (when out-chan (async/close! out-chan))
         (:memory next))))))
