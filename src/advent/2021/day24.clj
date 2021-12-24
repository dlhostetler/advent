(ns advent.2021.day24
  (:require [plumbing.core :refer :all]))

(def instructions
  [;; push 0, mn[0]
   [1 15 15]
   ;; push 1, mn[1]
   [1 12 5]
   ;; push 2, mn[2]
   [1 13 6]
   ;; pop 2, mn[3] = mn[2] + 6 - 14
   [26 -14 7]
   ;; push 3, mn[4]
   [1 15 9]
   ;; pop 3, mn[5] = mn[4] + 9 - 7
   [26 -7 6]
   ;; push 4, mn[6]
   [1 14 14]
   ;; push 5, mn[7]
   [1 15 3]
   ;; push 6, mn[8]
   [1 15 1]
   ;; pop 6, mn[9] = mn[8] + 1 - 7
   [26 -7 3]
   ;; pop 5, mn[10] = mn[7] + 3 - 8
   [26 -8 4]
   ;; pop 4, mn[11] = mn[6] + 14 - 7
   [26 -7 6]
   ;; pop 1, mn[12] = mn[1] + 5 - 5
   [26 -5 7]
   ;; pop 0, mn[13] = mn[0] + 15 - 10
   [26 -10 1]])

(defn build-digit [partial-model-number dest src]
  (let [w (nth partial-model-number src)
        [_ _ c] (nth instructions src)
        [_ b _] (nth instructions dest)]
    (assoc partial-model-number dest (+ w b c))))

(defn build-model-number [partial-model-number]
  (let [model-number (-> partial-model-number
                         (build-digit 3 2)
                         (build-digit 5 4)
                         (build-digit 9 8)
                         (build-digit 10 7)
                         (build-digit 11 6)
                         (build-digit 12 1)
                         (build-digit 13 0))]
    (when (every? #(<= 1 % 9) model-number) model-number)))

(defn execute [z [a b c w]]
  (let [x (if (not= (+ (mod z 26) b) w) 1 0)
        z (int (/ z a))
        z (* z (+ (* 25 x) 1))
        z (+ z (* (+ w c) x))]
    z))

(alter-var-root #'execute memoize)

(defn model-number? [model-number]
  (->> model-number
       (map conj instructions)
       (reduce execute 0)
       zero?))

(def digit-possibilities
  (range 1 10))

(defn ->partial-model-numbers []
  (for [x0 digit-possibilities
        x1 digit-possibilities
        x2 digit-possibilities
        x4 digit-possibilities
        x6 digit-possibilities
        x7 digit-possibilities
        x8 digit-possibilities]
    [x0 x1 x2 nil x4 nil x6 x7 x8 nil nil nil nil nil]))

(defn run []
  (->> (->partial-model-numbers)
       (some advent.2021.day24/build-model-number)
       (apply str)))
