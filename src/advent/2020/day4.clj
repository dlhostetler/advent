(ns advent.2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def required-ks [:byr
                  :iyr
                  :eyr
                  :hgt
                  :hcl
                  :ecl
                  :pid
                  #_:cid
                  ])

(defn passport-lines []
  (-> "resources/2020/day4.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn parse-field [field-str]
  (let [[k v] (str/split field-str #":")]
    [(keyword k) v]))

(defn parse-passport [line]
  (->> (str/split line #"\s+")
       (map parse-field)
       (into {})))

(defn valid? [passport]
  (every? #(contains? passport %) required-ks))

(defn run []
  (->> (passport-lines)
       (map parse-passport)
       (map valid?)
       (filter identity)
       count))
