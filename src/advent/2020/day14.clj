(ns advent.2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def init-state {:mask nil
                 :memory {}})

(defn n->bits [n]
  (let [s (Long/toBinaryString n)]
    (str (str/join (repeat (- 36 (count s)) "0")) s)))

(defn bits->n [address]
  (Long/parseLong address 2))

(defn parse-instruction [line]
  (cond
    (str/starts-with? line "mask")
    {:mask (-> line (str/split #" = ") last)
     :type :mask}
    (str/starts-with? line "mem")
    (let [[address value] (rest (re-matches #"mem\[(\d+)\] = (\d+)" line))]
      {:address (-> address Integer/parseInt n->bits)
       :type :mem
       :value (-> value Integer/parseInt n->bits)})
    :else
    (throw (Exception. (str "Unknown instruction: " line)))))

(defn instructions []
  (->> "resources/2020/day14.input"
       io/reader
       line-seq
       (map parse-instruction)))

(defn bit-mask [mask-v v]
  (if (= \X mask-v)
    (str v)
    (str mask-v)))

(defn masked [mask value]
  (-> (map bit-mask mask value)
      (str/join)))

(defmulti execute-instruction
          (fn [state instruction]
            (:type instruction)))

(defmethod execute-instruction :mask [state {:keys [mask]}]
  (assoc state :mask mask))

(defmethod execute-instruction :mem [state {:keys [address value]}]
  (assoc-in state
            [:memory address]
            (masked (:mask state) value)))

(defn run []
  (->> (instructions)
       (reduce execute-instruction init-state)
       :memory
       vals
       (map bits->n)
       (reduce +)))
