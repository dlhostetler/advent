(ns advent.2021.day10
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [plumbing.core :refer :all]))

(def to-ast
  (insta/parser "resources/2021/day10.grammar"))

(def char->score
  {\) 1
   \] 2
   \} 3
   \> 4})

(def valid-completion? #{")" "]" "}" ">"})

(def closing-char
  {"(" ")"
   "[" "]"
   "{" "}"
   "<" ">"})

(defn input []
  (->> "resources/2021/day10.input"
       io/reader
       line-seq))

(defn incomplete [ast]
  (let [failure (insta/get-failure ast)]
    (when (= (count (:text failure)) (-> failure :column dec))
      ast)))

(defn next-char [failure]
  (or (->> failure
           :reason
           (map :expecting)
           (filter valid-completion?)
           first)
      ;; because of the way the grammar is set up, it doesn't understand
      ;; completing a simple block
      (-> failure :text last str closing-char str)))

(defn completion [failure]
  (let [line (:text failure)]
    (loop [ast failure
           completion ""]
      (if (insta/failure? ast)
        (let [next-completion (str completion (next-char ast))
              next-line (str line next-completion)]
          (recur (to-ast next-line) next-completion))
        completion))))

(defn into-score [score char]
  (+ (* score 5) (char->score char)))

(defn completion->score [completion]
  (reduce into-score 0 completion))

(defn middle-value [v]
  (when-not (empty? v)
    (nth v (quot (count v) 2))))

(defn run []
  (let [lines (input)]
    (->> lines
         (map #(to-ast %))
         (map incomplete)
         (filter some?)
         (map completion)
         (map completion->score)
         sort
         middle-value)))
