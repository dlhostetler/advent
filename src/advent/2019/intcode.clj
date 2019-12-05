(ns advent.2019.intcode)

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

(defmulti execute-instruction current-instruction)

(defmethod execute-instruction :add [{:keys [pointer] :as state}]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)]
    (println (str "(" pointer ")") "Executing add of" arg0 arg1 "into" out-pos)
    (-> state
        (update :memory safe-assoc out-pos (+ arg0 arg1))
        (advance-pointer args))))

(defmethod execute-instruction :equals [{:keys [pointer] :as state}]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)]
    (println (str "(" pointer ")") "Executing equals of" arg0 arg1 "into" out-pos)
    (let [val (if (= arg0 arg1) 1 0)]
      (-> state
          (update :memory safe-assoc out-pos val)
          (advance-pointer args)))))

(defmethod execute-instruction :halt [{:keys [pointer] :as state}]
  (println (str "(" pointer ")") "Executing halt operation (no op).")
  (halt-pointer state))

(defmethod execute-instruction :input [{:keys [pointer] :as state}]
  (let [args [:out]
        [out-pos] (parse-args state args)]
    (println (str "(" pointer ")") "Executing input of" out-pos)
    (print "> ")
    (flush)
    (let [i (Integer/parseInt (read-line))]
      (-> state
          (update :memory safe-assoc out-pos i)
          (advance-pointer args)))))

(defmethod execute-instruction :jump-if-false [{:keys [pointer] :as state}]
  (let [args [:in :in]
        [arg0 arg1] (parse-args state args)]
    (println (str "(" pointer ")") "Executing jump if false for" arg0 arg1)
    (if (zero? arg0)
      (set-pointer state arg1)
      (advance-pointer state args))))

(defmethod execute-instruction :jump-if-true [{:keys [pointer] :as state}]
  (let [args [:in :in]
        [arg0 arg1] (parse-args state args)]
    (println (str "(" pointer ")") "Executing jump if true for" arg0 arg1)
    (if-not (zero? arg0)
      (set-pointer state arg1)
      (advance-pointer state args))))

(defmethod execute-instruction :less-than [{:keys [pointer] :as state}]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)]
    (println (str "(" pointer ")") "Executing less than of" arg0 arg1 "into" out-pos)
    (let [val (if (< arg0 arg1) 1 0)]
      (-> state
          (update :memory safe-assoc out-pos val)
          (advance-pointer args)))))

(defmethod execute-instruction :multiply [{:keys [memory pointer] :as state}]
  (let [args [:in :in :out]
        [arg0 arg1 out-pos] (parse-args state args)]
    (println (str "(" pointer ")")
             "Executing multiply of" arg0 arg1 "into" out-pos)
    (-> state
        (update :memory safe-assoc out-pos (* arg0 arg1))
        (advance-pointer args))))

(defmethod execute-instruction :output [{:keys [memory pointer] :as state}]
  (let [args [:out]
        [arg0] (parse-args state args)]
    (println (str "(" pointer ")") "Executing output of" arg0)
    (println (get memory arg0))
    (advance-pointer state args)))

;; Public
;; ======

(defn execute-instructions [memory]
  (loop [next {:memory memory
               :pointer 0}]
    (if-not (-> next :pointer neg?)
      (recur (execute-instruction next))
      (:memory next))))

(defn set-input [memory noun verb]
  (-> memory
      (assoc 1 noun)
      (assoc 2 verb)))

(defn calc [memory noun verb]
  (-> memory
      (set-input noun verb)
      execute-instructions
      (nth 0)))
