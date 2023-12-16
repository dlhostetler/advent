(ns advent.2023.day16
  (:require [advent.grid :as grid]
            [advent.seq :as seq]
            [clojure.java.io :as io]
            [plumbing.core :refer :all]))

(def init-contraption
  (-> "resources/2023/day16.input"
      io/reader
      grid/slurp))

(def dir->neighbor {:n grid/north
                    :e grid/east
                    :s grid/south
                    :w grid/west})

(def back-slash-bounces {:n :w
                         :e :s
                         :s :e
                         :w :n})

(def forward-slash-bounces {:n :e
                            :e :n
                            :s :w
                            :w :s})

(defn beam->dir [beam]
  (-> beam last last))

(defn beam-exists? [beams start]
  (->> beams
       (map first)
       (some (partial = start))))

(defmulti extend-beam (fn [state beam next-p]
                        (get (:contraption state) next-p)))

(defmethod extend-beam \. [_ beam next-p]
  [(conj beam [next-p (beam->dir beam)])])

(defmethod extend-beam \| [{:keys [beams]} beam next-p]
  (let [dir (beam->dir beam)]
    (if (or (= dir :n) (= dir :s))
      [(conj beam [next-p dir])]
      (-> [beam]
          (cond-> (not (beam-exists? beams [next-p :n]))
                  (conj [[next-p :n]]))
          (cond-> (not (beam-exists? beams [next-p :s]))
                  (conj [[next-p :s]]))))))

(defmethod extend-beam \- [{:keys [beams]} beam next-p]
  (let [dir (beam->dir beam)]
    (if (or (= dir :e) (= dir :w))
      [(conj beam [next-p dir])]
      (-> [beam]
          (cond-> (not (beam-exists? beams [next-p :e]))
                  (conj [[next-p :e]]))
          (cond-> (not (beam-exists? beams [next-p :w]))
                  (conj [[next-p :w]]))))))

(defmethod extend-beam \/ [_ beam next-p]
  (let [dir (beam->dir beam)]
    [(conj beam [next-p (forward-slash-bounces dir)])]))

(defmethod extend-beam \\ [_ beam next-p]
  (let [dir (beam->dir beam)]
    [(conj beam [next-p (back-slash-bounces dir)])]))

(alter-var-root #'extend-beam memoize)

(defn next-beams [{:keys [contraption] :as state} beam]
  (let [[p dir] (last beam)
        neighbor (dir->neighbor dir)
        next-p (grid/valid-point-or-nil contraption (neighbor p))]
    (if next-p
      (extend-beam state beam next-p)
      [beam])))

(defn next-state [{:keys [beams] :as state}]
  (let [next-beams (mapcat (partial next-beams state) beams)]
    (assoc state :beams next-beams)))

(defn beam->grid [beam]
  (->> beam
       (mapv first)
       (map (fn [p] [p \#]))
       (into {})))

(defn state->energized [state]
  (->> state
       :beams
       (apply concat)
       (map first)
       (into #{})
       count))

(defn print-state [{:keys [contraption beams] :as state}]
  (grid/print (apply merge contraption
                     (map beam->grid beams)))
  (println "energized" (state->energized state)))

(defn run []
  (->> (seq/successive next-state {:contraption init-contraption
                                   :beams #{[[[0 0] :s]]}})
       (drop 1000)
       first
       print-state))
