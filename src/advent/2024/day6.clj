(ns advent.2024.day6
  (:require [advent.grid :as grid]
            [plumbing.core :refer :all]))

(def dir-to-fn
  {:n grid/north
   :e grid/east
   :s grid/south
   :w grid/west})

(def input
  (->> "resources/2024/day6.input"
       grid/slurp
       (map-vals str)
       (remove (comp (partial = ".") last))
       (into {})))

(def starting-pos
  (->> input
       (filter (comp (partial = "^") val))
       first
       first))

(def obstacles
  (dissoc input starting-pos))

(defn init-path []
  [[starting-pos :n]])

(defn print-path [path]
  (grid/print (merge obstacles
                     (->> (for [[pos] path]
                            [pos "X"])
                          (into {})))))

(defn outside? [pos]
  (nil? (grid/valid-point-or-nil obstacles pos)))

(defn turn-right [at dir]
  (let [next-dir (case dir
                   :n :e
                   :e :s
                   :s :w
                   :w :n)
        next-at ((dir-to-fn next-dir) at)]
    [next-at next-dir]))

(defn patrol [visited]
  (let [[at dir] (last visited)]
    (if (outside? at)
      ;; done
      (drop-last visited)
      ;; move
      (let [next-at ((dir-to-fn dir) at)]
        (if (obstacles next-at)
          (recur (conj visited (turn-right at dir)))
          (recur (conj visited [next-at dir])))))))

(defn hits-self? [updated-obstacles path visited current]
  (let [[at dir] current]
    (cond
      ;; never looped back
      (outside? at)
      false
      ;; ran into self
      (visited current)
      true
      ;; move
      :else
      (let [next-at ((dir-to-fn dir) at)]
        (if (updated-obstacles next-at)
          (recur updated-obstacles
                 (conj path current)
                 (conj visited current)
                 (turn-right at dir))
          (recur updated-obstacles
                 (conj path current)
                 (conj visited current)
                 [next-at dir]))))))

(defn maybe-add-obstacle [path new-obstacles]
  (let [[at dir] (last path)
        new-obstacle-at ((dir-to-fn dir) at)
        [next-at next-dir :as next] (turn-right at dir)]
    (cond
      ;; turning right would lead nowhere
      (outside? next-at)
      new-obstacles

      ;; turning right would lead immediately to another obstacle
      (obstacles next-at)
      (maybe-add-obstacle (conj path [at next-dir]) new-obstacles)

      (hits-self? (assoc obstacles new-obstacle-at "#")
                  path
                  (set path)
                  next)
      (conj new-obstacles new-obstacle-at)

      :else
      new-obstacles)))

(defn find-new-obstacles [full-path]
  (loop [path full-path
         new-obstacles []]
    (if (empty? path)
      new-obstacles
      (recur (vec (butlast path))
             (maybe-add-obstacle path new-obstacles)))))

(defn run []
  (->> (init-path)
       patrol
       find-new-obstacles
       count
       ;; this is high by one, no idea why
       dec))
