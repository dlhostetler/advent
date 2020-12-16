(ns advent.2020.day16
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

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
       (mapv #(Integer/parseInt %))))

(defn init []
  (let [[rules-str
         ticket-str
         other-tickets-str] (-> "resources/2020/day16.input"
                                slurp
                                (str/split #"\n\n"))]
    {:rules (->> (str/split rules-str #"\n")
                 (map parse-rule)
                 set)
     :ticket (-> (str/split ticket-str #"\n")
                 last
                 parse-ticket)
     :tickets (->> (str/split other-tickets-str #"\n")
                   rest
                   (mapv parse-ticket))}))

(defn valid [{:keys [first-range second-range] :as rule} value]
  (when (or (<= (first first-range) value (second first-range))
            (<= (first second-range) value (second second-range)))
    rule))

(defn valid-rule [rules value]
  (some #(valid % value) rules))

(defn valid-ticket? [rules ticket]
  (every? #(valid-rule rules %) ticket))

(defn rule-for-index? [tickets rule idx]
  (->> tickets
       (map #(nth % idx))
       (every? #(valid rule %))))

(defn indices-for-rule [tickets rule]
  (->> (range (-> tickets first count))
       (filter #(rule-for-index? tickets rule %))
       set))

(defn one-to-one? [rules-by-index]
  (->> rules-by-index
       vals
       (every? #(= 1 (count %)))))

(defn rectify-index [rules-by-index idx]
  (->> (for [[rule-name indices] rules-by-index]
         [rule-name (if (> (count indices) 1)
                      (disj indices idx)
                      indices)])
       (into {})))

(defn reserved-indices [rules-by-index]
  (->> rules-by-index
       vals
       (filter #(= 1 (count %)))
       (map first)))

(defn rectify-indices [rules-by-index]
  (loop [rbi rules-by-index]
    (if-not (one-to-one? rbi)
      (recur (reduce rectify-index rbi (reserved-indices rbi)))
      rbi)))

(defn ticket->fields [index->rule ticket]
  (->> ticket
       (map-indexed vector)
       (map-keys index->rule)))

(defn run []
  (let [{:keys [rules ticket tickets]} (init)
        valid-tickets (filter #(valid-ticket? rules %) tickets)
        index->rule (->> (zipmap (map :name rules)
                                 (map #(indices-for-rule valid-tickets %)
                                      rules))
                         rectify-indices
                         (map-vals first)
                         set/map-invert)]
    (->> (ticket->fields index->rule ticket)
         (filter #(-> % key (str/starts-with? "departure")))
         (map last)
         (reduce *))))
