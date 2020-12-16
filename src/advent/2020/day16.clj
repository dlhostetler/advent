(ns advent.2020.day16
  (:require [clojure.string :as str]))

(defn parse-rule [rule-str]
  (let [[name
         first-range-from
         first-range-to
         second-range-from
         second-range-to] (rest (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" rule-str))]
    {:name name
     :first-range [(Integer/parseInt first-range-from)
                   (Integer/parseInt first-range-to)]
     :second-range [(Integer/parseInt second-range-from)
                    (Integer/parseInt second-range-to)]}))

(defn parse-ticket [ticket-str]
  (->> (str/split ticket-str #",")
       (map #(Integer/parseInt %))))

(defn init-state []
  (let [[rules-str
         ticket-str
         other-tickets-str] (-> "resources/2020/day16.input"
                                slurp
                                (str/split #"\n\n"))]
    {:rules (->> (str/split rules-str #"\n")
                 (map parse-rule))
     :ticket (-> (str/split ticket-str #"\n")
                 last
                 parse-ticket)
     :tickets (->> (str/split other-tickets-str #"\n")
                   rest
                   (map parse-ticket))}))

(defn valid [{:keys [first-range second-range] :as rule} value]
  (when (or (<= (first first-range) value (second first-range))
            (<= (first second-range) value (second second-range)))
    rule))

(defn valid-rule [rules value]
  (some #(valid % value) rules))

(defn run []
  (let [{:keys [rules tickets]} (init-state)]
    (->> tickets
         flatten
         (remove #(valid-rule rules %))
         (reduce +))))
