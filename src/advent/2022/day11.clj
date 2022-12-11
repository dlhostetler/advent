(ns advent.2022.day11
  (:require [advent.seq :as seq]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn str-is-number? [s]
  (every? #(Character/isDigit ^char %) s))

(defn new-tx [operation]
  (let [[_ op-str operand-str] (re-matches #".+new = old ([+*]) (\w+)"
                                           operation)
        op (if (= op-str "+") + *)
        n (when (str-is-number? operand-str) (Integer/parseInt operand-str))]
    (if n
      (fn [old]
        (op old n))
      (fn [old] (op old old)))))

(defn new-next-monkey [test-str tests-true-str tests-false-str]
  (let [divisible-by (-> test-str
                         (str/split #" ")
                         last
                         Integer/parseInt)
        monkey-if-true (-> tests-true-str
                           (str/split #" ")
                           last
                           Integer/parseInt)
        monkey-if-false (-> tests-false-str
                            (str/split #" ")
                            last
                            Integer/parseInt)]
    (fn [n]
      (if (zero? (mod n divisible-by))
        monkey-if-true
        monkey-if-false))))

(defn parse-monkey [i s]
  (let [[_ items operation test tests-true tests-false] (str/split s #"\n")]
    {:inspected 0
     :items (->> (-> items
                     (str/split #":")
                     last
                     (str/split #","))
                 (map str/trim)
                 (mapv #(Integer/parseInt %)))
     :next-monkey (new-next-monkey test tests-true tests-false)
     :num i
     :tx (new-tx operation)}))

(def init-state
  (->> (-> "resources/2022/day11.input"
           io/reader
           slurp
           (str/split #"\n\n"))
       (map-indexed parse-monkey)
       (into [])))

(defn item-turn [{from-monkey :num next-monkey :next-monkey tx :tx} state item]
  (let [next-item (-> item
                      tx
                      (/ 3)
                      long)
        to-monkey (next-monkey next-item)]
    (-> state
        (update-in [from-monkey :inspected] inc)
        (update-in [from-monkey :items] (comp vec rest))
        (update-in [to-monkey :items] conj next-item))))

(defn turn [state monkey-num]
  (let [monkey (nth state monkey-num)]
    (reduce (partial item-turn monkey)
           state
           (:items monkey))))

(defn round [state]
  (reduce turn state (map :num state)))

(defn run []
  (->> init-state
       (seq/successive round)
       (drop 1)
       (take 20)
       last
       (map :inspected)
       sort
       reverse
       (take 2)
       (reduce *)))
