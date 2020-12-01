(ns advent.2019.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-ingredient [m]
  (-> m
      str/trim
      (str/split #" ")
      vec
      (update 0 #(Integer/parseInt %))
      (update 1 keyword)))

(defn parse-reaction [l]
  (let [[in out] (str/split l #"=>")
        [quantity material] (parse-ingredient out)]
    [material {:quantity quantity
               :inputs (mapv parse-ingredient (str/split in #","))}]))

(def reactions
  (->> "resources/2019/day14.input"
       io/reader
       line-seq
       (mapv parse-reaction)
       (into {})))

(declare produce)

(defn produce-inputs [state [target-quantity target-material :as target] available]
  (let [{:keys [inputs quantity]} (get reactions target-material)
        multiplier (long (Math/ceil (/ (- target-quantity available) quantity)))]
    (println "multiplier" (- target-quantity available) quantity multiplier)
    (doseq [[input-quantity input-material] inputs]
      (produce state [(* multiplier input-quantity) input-material])
      (swap! state
             update-in
             [:inventory input-material]
             -
             (* multiplier input-quantity))
      (swap! state
             update-in
             [:consumed input-material]
             (fnil + 0)
             (* multiplier input-quantity))
      (println "consumed" [(* multiplier input-quantity) input-material] @state))))

(defn produce [state [target-quantity target-material :as target]]
  (println "need" target @state)
  (let [available (get-in @state [:inventory target-material] 0)]
    (println "available" available)
    (when (< available target-quantity)
      (if (= :ORE target-material)
        (swap! state
               update-in
               [:inventory :ORE]
               (fnil + 0)
               target-quantity)
        (let [{:keys [quantity]} (get reactions target-material)
              multiplier (long (Math/ceil (/ (- target-quantity available) quantity)))]
          (produce-inputs state target available)
          (swap! state
                 update-in
                 [:inventory target-material]
                 (fnil + 0)
                 (* multiplier quantity))
          (println "produced" [(* multiplier quantity) target-material] @state)
          @state)))))