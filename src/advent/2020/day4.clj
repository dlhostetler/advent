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
                  #_:cid])

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

(defmulti valid-field? (fn [k v] k))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
(defmethod valid-field? :byr [k v]
  (and (re-matches #"\d{4}" v)
       (<= 1920 (Integer/parseInt v) 2002)))

;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(defmethod valid-field? :iyr [k v]
  (and (re-matches #"\d{4}" v)
       (<= 2010 (Integer/parseInt v) 2020)))

;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(defmethod valid-field? :eyr [k v]
  (and (re-matches #"\d{4}" v)
       (<= 2020 (Integer/parseInt v) 2030)))

;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
(defmethod valid-field? :hgt [k v]
  (and (re-matches #"\d+(cm|in)" v)
       (or (and (str/ends-with? v "cm")
                (<= 150 (-> v (str/replace "cm" "") (Integer/parseInt)) 193))
           (and (str/ends-with? v "in")
                (<= 59 (-> v (str/replace "in" "") (Integer/parseInt)) 76)))))

;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(defmethod valid-field? :hcl [k v]
  (re-matches #"#[0-9a-f]{6}" v))

;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(defmethod valid-field? :ecl [k v]
  (println "ecl" v)
  (let [colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}]
    (some (partial = v) colors)))

;pid (Passport ID) - a nine-digit number, including leading zeroes.
(defmethod valid-field? :pid [k v]
  (re-matches #"\d{9}" v))

(defn valid? [passport]
  (and (every? #(and (contains? passport %)
                     (valid-field? % (get passport %)))
               required-ks)))

(defn run []
  (->> (passport-lines)
       (map parse-passport)
       (map valid?)
       (filter identity)
       count))
