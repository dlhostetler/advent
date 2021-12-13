(ns advent.2021.day13
  (:require [advent.grid :as grid]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def input
  (-> "resources/2021/day13.input"
      io/reader
      slurp
      (str/split #"\n\n")))

(defn parse-coords [[x-str y-str]]
  [[(Integer/parseInt x-str) (Integer/parseInt y-str)] "#"])

(def full-paper
  (->> (-> input first (str/split #"\n"))
       (map #(str/split % #","))
       (map parse-coords)
       (into {})))

(defn parse-fold [line]
  (let [[_ dir at] (re-matches #"fold along ([xy])=(\d+)" line)]
    [(keyword dir) (Integer/parseInt at)]))

(def all-folds
  (->> (-> input last (str/split #"\n"))
       (map parse-fold)))

(defn print-paper [paper]
  (grid/print paper identity {:padding 0
                              :y-dir :top-down}))

(defn folded-past [k folded-at]
  (fn [[point]]
    (> (nth point k) folded-at)))

(defn flip-point [point k amt]
  (-> point
      (update k #(- amt %))
      (update k + amt)))

(defn fold [paper [dir at]]
  (let [point-k (if (= dir :x) 0 1)
        bottom (->> paper
                    (filter (folded-past point-k at))
                    (into {}))
        top (->> paper
                 (remove #(contains? bottom (key %)))
                 (into {}))
        bottom (map-keys #(flip-point % point-k at) bottom)]
    (merge top bottom)))

(defn run []
  (print-paper (reduce fold full-paper all-folds)))
