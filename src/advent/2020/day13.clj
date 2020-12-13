(ns advent.2020.day13
  (:require [clojure.string :as str]
            [plumbing.core :refer :all]))

(def buses-raw "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19")

(defn parse-buses []
  (->> (str/split buses-raw #",")
       (map-indexed vector)
       (into {})
       (remove (comp (partial = "x") val))
       (map-vals #(Integer/parseInt %))))

(defn minutes-left [minValue offset bus-id]
  (mod (+ minValue offset) bus-id))

(defn run []
  (let [start-time (atom 0)
        product (atom 1)]
    (doseq [[bus-index bus-id] (parse-buses)]
      (while (not (zero? (minutes-left @start-time bus-index bus-id)))
        (swap! start-time + @product))
      (swap! product * bus-id))
    @start-time))
