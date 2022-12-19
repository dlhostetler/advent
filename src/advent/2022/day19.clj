(ns advent.2022.day19
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [plumbing.core :refer :all]))

(defn parse-blueprint [line]
  (let [[_
         blueprint-num
         ore-robot-cost-ore
         clay-robot-cost-ore
         obsidian-robot-cost-ore
         obsidian-robot-cost-clay
         geode-robot-cost-ore
         geode-robot-cost-obsidian] (re-matches #"Blueprint (.+): Each ore robot costs (.+) ore. Each clay robot costs (.+) ore. Each obsidian robot costs (.+) ore and (.+) clay. Each geode robot costs (.+) ore and (.+) obsidian." line)]
    {:costs {:clay {:ore (Integer/parseInt clay-robot-cost-ore)}
             :geode {:obsidian (Integer/parseInt geode-robot-cost-obsidian)
                     :ore (Integer/parseInt geode-robot-cost-ore)}
             :obsidian {:clay (Integer/parseInt obsidian-robot-cost-clay)
                        :ore (Integer/parseInt obsidian-robot-cost-ore)}
             :ore {:ore (Integer/parseInt ore-robot-cost-ore)}}
     :ingredients {:clay 0
                   :geode 0
                   :obsidian 0
                   :ore 0}
     :num (Integer/parseInt blueprint-num)
     :robots {:clay 0
              :geode 0
              :obsidian 0
              :ore 1}}))

(def init-blueprints
  (->> "resources/2022/day19.input"
       io/reader
       line-seq
       (map parse-blueprint)))

(defn collect-ingredients [blueprint]
  (update blueprint
          :ingredients
          (partial merge-with +)
          (:robots blueprint)))

(defn need-robot? [costs robots type]
  (let [max-cost (->> costs
                      vals
                      (map #(get % type))
                      (remove nil?)
                      (reduce max 0))]
    (< (get robots type) max-cost)))

(defn can-afford? [ingredients cost]
  (->> (for [[ingredient n] cost]
         (>= (get ingredients ingredient) n))
       (every? true?)))

(defn start-robot [blueprint type]
  (let [cost (get-in blueprint [:costs type])]
    (when (can-afford? (:ingredients blueprint) cost)
      (-> blueprint
          (update :ingredients (partial merge-with -) cost)
          (assoc :pending type)))))

(defn start-robots [{costs :costs
                     ingredients :ingredients
                     robots :robots
                     :as blueprint}]
  (cond
    (can-afford? ingredients (get costs :geode))
    [(start-robot blueprint :geode)]

    :else
    (->> [(when (< (get ingredients :ore) 4) blueprint)
          (when (need-robot? costs robots :obsidian) (start-robot blueprint :obsidian))
          (when (need-robot? costs robots :ore) (start-robot blueprint :ore))
          (when (need-robot? costs robots :clay) (start-robot blueprint :clay))]
         (remove nil?))))

(defn finish-robot [blueprint]
  (if-let [type (:pending blueprint)]
    (-> blueprint
        (update-in [:robots type] inc)
        (dissoc :pending))
    blueprint))

(defn max-geodes [blueprints0 blueprints1]
  (let [blueprint0 (last blueprints0)
        blueprint1 (last blueprints1)]
    (cond
      (nil? blueprint0)
      blueprints1
      (nil? blueprint1)
      blueprints0
      (>= (get-in blueprint0 [:ingredients :geode])
          (get-in blueprint1 [:ingredients :geode]))
      blueprints0
      :else
      blueprints1)))

(defn blueprint->max-geodes [minutes blueprints]
  (if (zero? minutes)
    blueprints

    (let [blueprint (last blueprints)]
      (->> (start-robots blueprint)
           (map collect-ingredients)
           (map finish-robot)
           (map #(blueprint->max-geodes (dec minutes) (conj blueprints %)))
           (reduce max-geodes)))))

(defn geodes [blueprint]
  (get-in blueprint [:ingredients :geode]))

(defn run []
  (->> init-blueprints
       (take 3)
       (map vector)
       (pmap (partial blueprint->max-geodes 32))
       (map last)
       (map geodes)
       (reduce *)))
