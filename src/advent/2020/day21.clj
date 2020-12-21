(ns advent.2020.day21
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-food [line]
  (let [[ingredients allergens] (str/split line #" \(contains ")]
    {:allergens (-> allergens
                    (subs 0 (-> allergens count dec))
                    str/trim
                    (str/split #", "))
     :ingredients (-> ingredients
                      str/trim
                      (str/split #" "))}))

(defn parse-foods []
  (->> "resources/2020/day21.input"
       io/reader
       line-seq
       (map parse-food)))

(defn allergens-by-ingredient [allergens ingredient]
  {ingredient allergens})

(defn by-allergen [acc {:keys [allergens ingredients]}]
  (->> (map #(allergens-by-ingredient allergens %) ingredients)
       (apply merge-with into acc)))

(defn invalid? [required-freqs [ingredient freq]]
  (not= (get required-freqs ingredient) freq))

(defn remove-invalids [required-freqs ingredient-freqs]
  (->> ingredient-freqs
       (remove #(invalid? required-freqs %))
       (into {})))

(defn one-to-one? [rules-by-index]
  (->> rules-by-index
       vals
       (every? #(= 1 (count %)))))

(defn rectify-allergen [ingredient->allergens allergen]
  (->> (for [[ingredient allergens] ingredient->allergens]
         [ingredient (if (> (count allergens) 1)
                       (disj allergens allergen)
                       allergens)])
       (into {})))

(defn done-allergens [ingredient->allergens]
  (->> ingredient->allergens
       vals
       (filter #(= 1 (count %)))
       (map first)))

(defn rectify-allergens [ingredient->allergens]
  (loop [i->a ingredient->allergens]
    (if-not (one-to-one? i->a)
      (recur (reduce rectify-allergen i->a (done-allergens i->a)))
      i->a)))

(defn run []
  (let [foods (parse-foods)
        allergen-freqs (->> foods
                            (mapcat :allergens)
                            frequencies)
        ingredient->allergens (->> (reduce by-allergen {} foods)
                                   (map-vals frequencies)
                                   (map-vals #(remove-invalids allergen-freqs %))
                                   (map-vals keys))
        safe-ingredients (->> ingredient->allergens
                              (remove val)
                              (map key)
                              set)
        unsafe-ingredients (set/difference (-> ingredient->allergens
                                               keys
                                               set)
                                           safe-ingredients)
        allergen->ingredient (->> (select-keys ingredient->allergens unsafe-ingredients)
                                  (map-vals set)
                                  (rectify-allergens)
                                  (map-vals first)
                                  set/map-invert)]
    (->> allergen->ingredient
         keys
         sort
         (map allergen->ingredient)
         (str/join ","))))
