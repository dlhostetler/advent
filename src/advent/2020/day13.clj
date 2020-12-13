(ns advent.2020.day13
  (:require [clojure.string :as str]))

(def buses-raw "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19")

(defn buses []
  (->> (str/split buses-raw #",")
       (remove (partial = "x"))
       (map #(Integer/parseInt %))))

(defn bus-after [arrival bus-id]
  [(-> (/ arrival bus-id)
       int
       inc
       (* bus-id))
   bus-id])

(defn run []
  (let [my-arrival 1000655
        id-by-arrival (->> (buses)
                           (map (partial bus-after my-arrival))
                           (into {}))
        target-arrival (->> id-by-arrival
                            (map key)
                            (apply min))
        bus-id (get id-by-arrival target-arrival)]
    (* (- target-arrival my-arrival)
       bus-id)))
