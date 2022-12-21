(ns advent.2022.day21
  (:require [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def operator->fn
  {"+" +
   "-" -
   "*" *
   "/" /})

(defn parse-monkey [line]
  (let [[_ name num-or-operand0 operator operand1]
        (re-matches #"(.+): ([a-z0-9]+) ?(.)? ?(.+)?" line)]
    (cond
      (= name "root")
      {:name name
       :fn (fn [x y]
             (println "x" (long x) "y" (long y) "x-y=" (- (long x) (long y)))
             (= x y))
       :operand0 num-or-operand0
       :operand1 operand1}

      operator
      {:name name
       :fn (operator->fn operator)
       :operand0 num-or-operand0
       :operand1 operand1}

      :else
      {:name name
       :num (Integer/parseInt num-or-operand0)})))

(def monkeys
  (->> "resources/2022/day21.input"
       io/reader
       line-seq
       (map parse-monkey)
       (remove (comp (partial = "humn") :name))))

(defn ->promises [n]
  (-> (into {}
         (for [{name :name} monkeys]
           [name (promise)]))
      (assoc "humn" (deliver (promise) n))))

(defn do-monkey [promises {name :name :as monkey}]
  (let [p (get promises name)]
    (if (:num monkey)
      (deliver p (:num monkey))
      (deliver p ((:fn monkey)
                  (-> promises (get (:operand0 monkey)) deref)
                  (-> promises (get (:operand1 monkey)) deref))))))

(defn try-humn [n]
  (let [promises (->promises n)]
    (doseq [m monkeys]
      (future (do-monkey promises m)))
    [n (-> promises (get "root") deref)]))

(defn run []
  (try-humn 3247317268284))
