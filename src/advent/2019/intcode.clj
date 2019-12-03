(ns advent.2019.intcode)

(def instruction->keyword
  {1 :add
   2 :multiply
   99 :halt})

(def num-instruction-vals
  {:add 4
   :halt 0
   :multiply 4})

(defn- current-instruction [memory pointer]
  (instruction->keyword (nth memory pointer)))

(defn- advance-pointer [memory pointer]
  (let [instruction (current-instruction memory pointer)]
    (if (= :halt instruction)
      ;; the halt instruction is special in that it moves the pointer to indicate
      ;; stoppage
      -1
      (+ pointer (num-instruction-vals instruction)))))

(defn- deref-param [memory pointer]
  (let [memory-index (nth memory pointer)]
    (nth memory memory-index)))

(defn- inputs [memory pointer num-inputs]
  (into [] (for [offset (range num-inputs)]
             (deref-param memory (+ pointer offset 1)))))

(defn output-positions [memory pointer num-inputs num-outputs]
  (into [] (for [offset (range num-outputs)]
             (nth memory (+ pointer num-inputs offset 1)))))

(defmulti execute-instruction current-instruction)

(defmethod execute-instruction :add [memory pointer]
  (let [[arg0 arg1] (inputs memory pointer 2)
        [out-pos] (output-positions memory pointer 2 1)]
    (println (str "(" pointer ")") "Executing add of" arg0 arg1 "into" out-pos)
    (assoc memory out-pos (+ arg0 arg1))))

(defmethod execute-instruction :halt [memory pointer]
  (println (str "(" pointer ")") "Executing halt operation (no op).")
  memory)

(defmethod execute-instruction :multiply [memory pointer]
  (let [[arg0 arg1] (inputs memory pointer 2)
        [out-pos] (output-positions memory pointer 2 1)]
    (println (str "(" pointer ")")
             "Executing multiply of" arg0 arg1 "into" out-pos)
    (assoc memory out-pos (* arg0 arg1))))

;; Public
;; ======

(defn execute-instructions [memory]
  (loop [next memory
         pointer 0]
    (if-not (neg? pointer)
      (recur (execute-instruction next pointer)
             (advance-pointer next pointer))
      next)))

(defn set-input [memory noun verb]
  (-> memory
      (assoc 1 noun)
      (assoc 2 verb)))

(defn calc [memory noun verb]
  (-> memory
      (set-input noun verb)
      execute-instructions
      (nth 0)))
