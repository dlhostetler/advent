(ns advent.seq)


(defn successive
  "Create a lazy infinite sequence by repeatedly calling f on each new state of x."
  [f x & args]
  (lazy-seq
    (cons x (apply successive
                   f
                   (apply f x args)
                   args))))
