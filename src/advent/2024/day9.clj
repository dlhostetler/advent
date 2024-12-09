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
         memory {}]
    (if (>= i (count input))
      memory
      (let [n (first ns)
            to (+ address n)]
        (if (even? i)
          (recur (rest ns)
                 (inc i)
                 (inc id)
                 to
                 (assoc memory id [address to]))
          (recur (rest ns)
                 (inc i)
                 id
                 to
                 (update memory nil (fnil conj []) [address to])))))))

(defn find-empty-block [memory size]
  (first (for [[empty-from empty-to :as empty-block] (get memory nil)
               :let [empty-size (- empty-to empty-from)]
               :when (<= size empty-size)]
           empty-block)))

(defn normalize-empties [blocks]
  (loop [blocks (vec (sort blocks))
         i 0]
    (if (>= i (-> blocks count dec))
      blocks
      (let [[from0 to0] (get blocks i)
            [from1 to1] (get blocks (inc i))]
        (if (= to0 from1)
          (recur (vec (concat (subvec blocks 0 i)
                              [[from0 to1]]
                              (subvec blocks (+ i 2) (count blocks))))
                 i)
          (recur blocks (inc i)))))))

(defn fill-empty [memory id [empty-from empty-to :as empty-block]]
  (let [[from to :as block] (get memory id)
        size (- to from)
        empty-size (- empty-to empty-from)]
    (cond
      ;; this shouldn't move because it would move right
      (> empty-from from)
      memory

      ;; this is an exact fit
      (= empty-size size)
      (-> memory
          (assoc nil (->> (get memory nil)
                          (remove (partial = empty-block))))
          (update nil conj block)
          (update nil normalize-empties)
          (assoc id empty-block))

      ;; otherwise break it down
      :else
      (-> memory
          (assoc nil (->> (get memory nil)
                          (remove (partial = empty-block))))
          (update nil conj block)
          (update nil conj [(+ empty-from size) empty-to])
          (update nil normalize-empties)
          (assoc id [empty-from (+ empty-from size)])))))

(defn adjust-block [memory id]
  (let [[from to] (get memory id)
        size (- to from)
        empty-block (find-empty-block memory size)]
    (if empty-block
      (fill-empty memory id empty-block)
      ;; no empty block found
      memory)))

(defn fill-memory [memory]
  (loop [memory memory
         id (->> memory keys (remove nil?) (apply max))]
    (if (zero? id)
      memory
      (recur (adjust-block memory id) (dec id)))))

(defn block-checksum [[id [from to]]]
  (loop [i from
         total 0]
    (if (>= i to)
      total
      (recur (inc i) (+ total (* id i))))))

(defn checksum [memory]
  (->> memory
       (remove (comp nil? key))
       (map block-checksum)
       (reduce +)))

(defn run []
  (checksum (fill-memory (init-memory))))
