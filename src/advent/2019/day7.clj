(ns advent.2019.day7
  (:require [advent.2019.intcode :as intcode]
            [clojure.core.async :as async :refer [<!! >!!]]
            [clojure.math.combinatorics :as combo]))

(def input
  [3 8 1001 8 10 8 105 1 0 0 21 38 63 88 97 118 199 280 361 442 99999 3 9 1002 9 3 9 101 2 9 9 1002 9 4 9 4 9 99 3 9 101 3 9 9 102 5 9 9 101 3 9 9 1002 9 3 9 101 3 9 9 4 9 99 3 9 1002 9 2 9 1001 9 3 9 102 3 9 9 101 2 9 9 1002 9 4 9 4 9 99 3 9 102 2 9 9 4 9 99 3 9 102 4 9 9 101 5 9 9 102 2 9 9 101 5 9 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 99 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 1 9 9 4 9 99 3 9 101 1 9 9 4 9 3 9 101 1 9 9 4 9 3 9 101 1 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 2 9 4 9 99])

(def test-input
  [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0])

(defn amplifier [id memory phase-setting in-chan out-chan]
  (let [f (future (intcode/execute-instructions id
                                                memory
                                                (intcode/chan->in in-chan)
                                                (intcode/chan->out out-chan)
                                                (intcode/halt-chans out-chan)))]
    (>!! in-chan phase-setting)
    f))

(defn thruster-signal [memory phase-settings]
  (let [a-chan (async/chan)
        b-chan (async/chan)
        c-chan (async/chan)
        d-chan (async/chan)
        e-chan (async/chan)
        a (amplifier :a memory (nth phase-settings 0) e-chan a-chan)
        b (amplifier :b memory (nth phase-settings 1) a-chan b-chan)
        c (amplifier :c memory (nth phase-settings 2) b-chan c-chan)
        d (amplifier :d memory (nth phase-settings 3) c-chan d-chan)
        e (amplifier :e memory (nth phase-settings 4) d-chan e-chan)]
    ;; write first input signal
    (>!! e-chan 0)
    ;; wait for completion
    @a
    (println "A finished.")
    @b
    (println "B finished.")
    @c
    (println "C finished.")
    @d
    (println "D finished.")
    (let [signal (<!! e-chan)]
      @e
      (println "E finished.")
      ;; return output
      signal)))

(defn run []
  (let [all-phase-settings (combo/permutations #{5 6 7 8 9})]
    (->> all-phase-settings
         (map thruster-signal (repeat input))
         (apply max))))