(ns advent.2024.day9
  (:require [clojure.java.io :as io]))

(def input
  (->> (-> "resources/2024/day9.input"
           io/reader
           slurp
           seq)
       (map str)
       (map #(Integer/parseInt %))))

(defn init-memory []
  (loop [ns input
         i 0
         id 0
         address 0
         memory []]
    (if (>= i (count input))
      memory
      (let [n (first ns)
            to (+ address n)]
        (if (even? i)
          (recur (rest ns)
                 (inc i)
                 (inc id)
                 to
                 (conj memory [address to id]))
          (recur (rest ns)
                 (inc i)
                 id
                 to
                 (conj memory [address to nil])))))))

(defn remove-empty [memory]
  (let [[_ _ id] (last memory)]
    (if (and (not (empty? memory)) (nil? id))
      (recur (vec (drop-last memory)))
      memory)))

(defn fill-empty [memory i empty-from empty-to]
  (loop [memory (remove-empty memory)
         empty-from empty-from
         need (- empty-to empty-from)
         to-fill []]
    (if (or (zero? need) (>= i (count memory)))
      (assoc memory i to-fill)
      (let [[file-from file-to id] (last memory)
            available (- file-to file-from)]
        (if (> available need)
          ;; more than enough to fill
          (recur (-> memory
                     drop-last
                     vec
                     (conj [file-from (- file-to need) id]))
                 empty-from
                 0
                 (conj to-fill [empty-from empty-to id]))
          ;; exactly enough or not enough to fill
          (recur (-> memory drop-last vec remove-empty)
                 (+ empty-from available)
                 (- need available)
                 (conj to-fill [empty-from (+ empty-from available) id])))))))

(defn fill-memory [memory i]
  (if (>= i (count memory))
    memory
    (let [[from to id] (get memory i)]
      (if (nil? id)
        (recur (fill-empty memory i from to) (inc i))
        ;; already filled
        (recur memory (inc i))))))

(defn coerce-nested [block]
  (if (vector? (first block))
    block
    [block]))

(defn normalize [memory]
  (->> memory
       (map coerce-nested)
       (apply concat)
       (remove empty?)
       vec))

(defn block-checksum [[from to id]]
  (loop [i from
         total 0]
    (if (>= i to)
      total
      (recur (inc i) (+ total (* id i))))))

(defn checksum [memory]
  (->> memory
       (map block-checksum)
       (reduce +)))

(defn run []
  (->> (fill-memory (init-memory) 0)
       normalize
       checksum))
