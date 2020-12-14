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

(defn set-bit [address n bit]
  (str (subs address 0 n)
       bit
       (subs address (inc n))))

(defn mask-bit [mask address n]
  (case (nth mask n)
    \X
    [(set-bit address n 0)
     (set-bit address n 1)]
    \0
    [address]
    \1
    [(set-bit address n 1)]))

(defn masked-addresses [mask address]
  (reduce (fn [addresses n]
            (mapcat #(mask-bit mask % n) addresses))
          [address]
          (range (count mask))))

(defmulti execute-instruction
          (fn [state instruction]
            (:type instruction)))

(defmethod execute-instruction :mask [state {:keys [mask]}]
  (assoc state :mask mask))

(defmethod execute-instruction :mem [state {:keys [address value]}]
  (reduce (fn [s a] (assoc-in s [:memory a] value))
          state
          (masked-addresses (:mask state) address)))

(defn run []
  (->> (instructions)
       (reduce execute-instruction init-state)
       :memory
       vals
       (map bits->n)
       (reduce +)))
