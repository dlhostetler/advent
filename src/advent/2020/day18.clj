(ns advent.2020.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.tx]
            [plumbing.core :refer :all]))

(def to-ast
  (insta/parser "resources/2020/day18.grammar"))

(defn input []
  (->> "resources/2020/day18.input"
       io/reader
       line-seq))

(def ast->result
  {:add (fn [x _ y] (+ x y))
   :multiply (fn [x _ y] (* x y))
   :number read-string})

(defn mathify [s]
  (let [ast (-> s
                (str/replace #"\s+" "")
                to-ast
                first)]
    (insta.tx/transform ast->result ast)))

(defn run []
  (->> (input)
       (map mathify)
       (reduce +)))
