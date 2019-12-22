(ns advent.2019.day21
  (:require [advent.2019.intcode :as intcode]))

(defn comma-separate [input]
  (interpose \, input))

(defn asciify-one [x]
  (cond
    (= \n x)
    10
    (char? x)
    (int x)
    (int? x)
    (->> x str seq (map int))
    :else
    (throw (ex-info "Not sure what x is." {:x x}))))

(defn asciify [& inputs]
  (->> inputs
       (map seq)
       (#(interleave % (repeat [\n])))
       ((fn [x] (println x) x))
       flatten
       (map asciify-one)
       flatten
       (into [])))

(defn run []
  (let [memory (intcode/file->memory "resources/day21.input")
        in (atom (asciify "NOT A J"
                          "NOT C T"
                          "AND H T"
                          "OR T J"
                          "NOT B T"
                          "AND A T"
                          "AND C T"
                          "OR T J"
                          "AND D J"
                          "RUN"))
        in (fn []
             (let [next (-> in deref first)]
               (swap! in rest)
               next))
        output (fn [x]
                 (if (= 10 x)
                   (println)
                   (print (char x))))]
    (intcode/execute-instructions :robot
                                  memory
                                  in
                                  output
                                  nil)
    nil))