(ns advent.seq)

;; from https://clojuredocs.org/clojure.core/partition-by
(defn partition-at
  "Like partition-by but will start a new run when f returns true"
  [f coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [run (cons (first s) (take-while #(not (f %)) (rest s)))]
        (cons run (partition-at f (drop (count run) s)))))))

(defn successive
  "Create a lazy infinite sequence by repeatedly calling f on each new state of x."
  [f x & args]
  (lazy-seq
    (cons x (apply successive
                   f
                   (apply f x args)
                   args))))

(defn take-while+1
  "The same as take-while, but includes the element that fails the check."
  [pred coll]
  #_(take-while pred coll)
  (reduce (fn [took v]
            (let [took+1 (conj took v)]
              (if (pred v) took+1 (reduced took+1))))
          (empty coll)
          coll))
