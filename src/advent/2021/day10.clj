(ns advent.2021.day10
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [plumbing.core :refer :all]))

(def to-ast
  (insta/parser "resources/2021/day10.grammar"))

(def char->score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn input []
  (->> "resources/2021/day10.input"
       io/reader
       line-seq))

(defn incomplete? [line failure]
  (= (count line) (-> failure :column dec)))

(defn ->corrupt-char [line ast]
  (let [failure (insta/get-failure ast)]
    (when (and failure (not (incomplete? line failure)))
      (nth line (-> failure :column dec)))))

(defn run []
  (let [lines (input)]
    (->> lines
         (map #(to-ast %))
         (map ->corrupt-char lines)
         (filter identity)
         (map char->score)
         (reduce +))))
