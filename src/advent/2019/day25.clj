(ns advent.2019.day25
  (:require [advent.2019.intcode :as intcode]))

(def prompt "Command?")
(def max-buffer-length (count prompt))

(defn next-char [buffer x]
  (let [next (str buffer x)]
    (cond-> next
            (> (count next) max-buffer-length)
            (subs 1))))

(defn run []
  (let [memory (intcode/file->memory "resources/day25.input")
        input (atom nil)
        in (fn []
             (let [next (-> input deref first)]
               (swap! input rest)
               next))
        output-buffer (atom "")
        output (fn [x]
                 (swap! output-buffer next-char (char x))
                 (cond
                   (= 10 x)
                   (println)
                   (= prompt @output-buffer)
                   (do
                     (print "? ")
                     (flush)
                     (->> (-> (read-line)
                              vec
                              (conj \newline))
                          (mapv int)
                          (reset! input)))
                   :else
                   (print (char x))))]
    (intcode/execute-instructions :droid
                                  memory
                                  in
                                  output
                                  nil)
    nil))