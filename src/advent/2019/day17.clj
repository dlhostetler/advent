(ns advent.2019.day17
  (:require [advent.2019.intcode :as intcode]
            [advent.2019.grid :as grid]))

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

(defn asciify [inputs]
  (->> inputs
       (map comma-separate)
       (#(interleave % (repeat [\n])))
       flatten
       (map asciify-one)
       flatten
       (into [])))

(defn run []
  (let [memory (intcode/file->memory "resources/2019/day17.input")
        in (atom (asciify [[\A \B \A \C \B \A \C \B \A \C]
                           #_A [\L 12 \L 12 \L 6 \L 6]
                           #_B [\R 8 \R 4 \L 12]
                           #_C [\L 12 \L 6 \R 12 \R 8]
                           [\y]]))
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
