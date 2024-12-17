(ns advent.2024.day17
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(defn jump2 [state]
  (update state :instruction + 2))

(defn output [state value]
  (update state :output conj value))

(defn register-val [state register]
  (get state register))

(defn set-instruction [state value]
  (assoc state :instruction value))

(defn set-register [state register value]
  (assoc state register value))

(defn combo-operand [state operand]
  (cond
    ;; Combo operands 0 through 3 represent literal values 0 through 3.
    (<= 0 operand 3)
    operand
    ;; Combo operand 4 represents the value of register A.
    (= operand 4)
    (register-val state :A)
    ;; Combo operand 5 represents the value of register B.
    (= operand 5)
    (register-val state :B)
    ;; Combo operand 6 represents the value of register C.
    (= operand 6)
    (register-val state :C)
    ;; Combo operand 7 is reserved and will not appear in valid programs.
    :else
    (throw (Exception. (str "unhandled combo operand " operand)))))

(defmulti op
  (fn [state opcode operand]
    opcode))

;; adv
(defmethod op 0 [state _ operand]
  (println "adv" operand)
  (-> state
      (set-register :A (int (/ (register-val state :A)
                               (math/pow 2 (combo-operand state operand)))))
      jump2))

;; bxl
(defmethod op 1 [state _ operand]
  (println "bxl" operand)
  (-> state
      (set-register :B (bit-xor (register-val state :B) operand))
      jump2))

;; bst
(defmethod op 2 [state _ operand]
  (println "bst" operand)
  (-> state
      (set-register :B (mod (combo-operand state operand) 8))
      jump2))

;; jnz
(defmethod op 3 [state _ operand]
  (println "jnz" operand)
  (let [a (register-val state :A)]
    (if (zero? a)
      (jump2 state)
      (set-instruction state operand))))

;; bxc
(defmethod op 4 [state _ _]
  (println "bxc")
  (-> state
      (set-register :B (bit-xor (register-val state :B) (register-val state :C)))
      jump2))

;; out
(defmethod op 5 [state _ operand]
  (println "out" operand)
  (-> state
      (output (mod (combo-operand state operand) 8))
      jump2))

;; bdv
(defmethod op 6 [state _ operand]
  (println "bdv" operand)
  (-> state
      (set-register :B (int (/ (register-val state :A)
                               (math/pow 2 (combo-operand state operand)))))
      jump2))

;; cdv
(defmethod op 7 [state _ operand]
  (println "cdv" operand)
  (-> state
      (set-register :C (int (/ (register-val state :A)
                               (math/pow 2 (combo-operand state operand)))))
      jump2))

(defn run-program [{:keys [instruction] :as state} program]
  (if (< instruction (count program))
    (recur (op state (nth program instruction) (nth program (inc instruction)))
           program)
    state))

(defn run []
  (->> (run-program {:A 66752888 :B 0 :C 0 :instruction 0 :output []}
                    [2,4,1,7,7,5,1,7,0,3,4,1,5,5,3,0])
       :output
       (str/join ",")))
