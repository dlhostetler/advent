(ns advent.2019.intcode)

(def instruction->keyword
  {1 :add
   2 :multiply
   3 :input
   4 :output
   99 :halt})

(def num-instruction-vals
  {:add 4
   :halt 0
   :input 2
   :output 2
   :multiply 4})

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn pad [v length pad-val]
  (let [n (- length (count v))]
    (if (pos? n)
      ;; need to extend the array
      (into v (repeat n pad-val))
      v)))

(defn- current-instruction [memory pointer]
  (-> (nth memory pointer)
      (mod 100)
      instruction->keyword))

(defn- advance-pointer [memory pointer]
  (let [instruction (current-instruction memory pointer)]
    (cond
      ;; the halt instruction is special in that it moves the pointer to indicate
      ;; stoppage
      (= :halt instruction)
      -1
      instruction
      (+ pointer (num-instruction-vals instruction))
      :else
      1)))

(defn- deref-param [memory pointer]
  (let [memory-index (nth memory pointer)]
    (nth memory memory-index)))

(defn safe-assoc [v i val]
  (assoc (pad v i 0) i val))

(defn- parse-args [memory pointer args]
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

(defmethod execute-instruction :add [memory pointer]
  (let [[arg0 arg1 out-pos] (parse-args memory pointer [:in :in :out])]
    (println (str "(" pointer ")") "Executing add of" arg0 arg1 "into" out-pos)
    (safe-assoc memory out-pos (+ arg0 arg1))))

(defmethod execute-instruction :halt [memory pointer]
  (println (str "(" pointer ")") "Executing halt operation (no op).")
  memory)

(defmethod execute-instruction :input [memory pointer]
  (let [[out-pos] (parse-args memory pointer [:out])]
    (println (str "(" pointer ")") "Executing input of" out-pos)
    (print "> ")
    (flush)
    (let [i (Integer/parseInt (read-line))]
      (safe-assoc memory out-pos i))))

(defmethod execute-instruction :multiply [memory pointer]
  (let [[arg0 arg1 out-pos] (parse-args memory pointer [:in :in :out])]
    (println (str "(" pointer ")")
             "Executing multiply of" arg0 arg1 "into" out-pos)
    (safe-assoc memory out-pos (* arg0 arg1))))

(defmethod execute-instruction :output [memory pointer]
  (let [[arg0] (parse-args memory pointer [:out])]
    (println (str "(" pointer ")") "Executing output of" arg0)
    (println (get memory arg0))
    memory))

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
