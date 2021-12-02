(ns advent.2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input []
  (-> "resources/2021/day2.input"
      io/reader
      slurp
      (str/split #"\n")))

(defn ->command [commandStr]
  (let [[command n] (str/split commandStr #" ")]
    {:command (keyword command)
     :amount (Integer/parseInt n)}))

(defmulti do-command
          (fn [acc command]
            (:command command)))

(defmethod do-command :down [acc {amount :amount}]
  (update acc :aim + amount))

(defmethod do-command :forward [acc {amount :amount}]
  (-> acc
      (update :horizontal + amount)
      (update :depth + (* amount (:aim acc)))))

(defmethod do-command :up [acc {amount :amount}]
  (update acc :aim - amount))

(defn run []
  (let [result (->> (input)
                    (map ->command)
                    (reduce do-command {:aim 0 :depth 0 :horizontal 0}))]
    (* (:horizontal result) (:depth result))))
