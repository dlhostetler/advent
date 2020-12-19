(ns advent.2020.day19
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [plumbing.core :refer :all]))

(def to-ast
  (insta/parser "resources/2020/day19.grammar"))

(defn input []
  (->> "resources/2020/day19.input"
       io/reader
       line-seq))

(defn run []
  (->> (input)
       (map to-ast)
       (remove insta/failure?)
       count))
