(ns advent.2019.day2)

(def broken-input
  [1 0 0 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 6 19 23 2 6 23 27 1 5 27 31 2 31 9 35 1 35 5 39 1 39 5 43 1 43 10 47 2 6 47 51 1 51 5 55 2 55 6 59 1 5 59 63 2 63 6 67 1 5 67 71 1 71 6 75 2 75 10 79 1 79 5 83 2 83 6 87 1 87 5 91 2 9 91 95 1 95 6 99 2 9 99 103 2 9 103 107 1 5 107 111 1 111 5 115 1 115 13 119 1 13 119 123 2 6 123 127 1 5 127 131 1 9 131 135 1 135 9 139 2 139 6 143 1 143 5 147 2 147 6 151 1 5 151 155 2 6 155 159 1 159 2 163 1 9 163 0 99 2 0 14 0])

(def input
  (-> broken-input
      (assoc 1 12)
      (assoc 2 2)))

(def add-op 1)
(def multiply-op 2)
(def sto-op 99)
(def op-length 4)

(defn advance-index [v i]
  (if (= sto-op (nth v i))
    -1
    (+ i op-length)))

(defn arg-deref [v i]
  (let [arg-index (nth v i)]
    (nth v arg-index)))

(defn op-inputs [v i]
  [(arg-deref v (+ i 1)) (arg-deref v (+ i 2))])

(defn op-output-pos [v i]
  (nth v (+ i 3)))

(defmulti execute-op
  (fn [v i]
    (nth v i)))

(defmethod execute-op add-op [v i]
  (let [[arg0 arg1] (op-inputs v i)
        out-pos (op-output-pos v i)]
    (println (str "(" i ")") "Executing add of" arg0 arg1 "into" out-pos)
    (assoc v out-pos (+ arg0 arg1))))

(defmethod execute-op multiply-op [v i]
  (let [[arg0 arg1] (op-inputs v i)
        out-pos (op-output-pos v i)]
    (println (str "(" i ")") "Executing multiply of" arg0 arg1 "into" out-pos)
    (assoc v out-pos (* arg0 arg1))))

(defmethod execute-op sto-op [v i]
  (println (str "(" i ")") "Executing stop operation (no op).")
  v)

(defn execute [input]
  (loop [i 0
         next input]
    (if-not (neg? i)
      (recur (advance-index next i) (execute-op next i))
      next)))

(defn run []
  (-> input
      execute
      (nth 0)))