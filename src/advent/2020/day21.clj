(ns advent.2020.day21
  (:require [clojure.java.io :as io]
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

(defn num-foods-for [foods ingredient]
  (->> foods
       (map :ingredients)
       (map set)
       (filter #(contains? % ingredient))
       count))

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
                              set)]
    (->> safe-ingredients
         (map #(num-foods-for foods %))
         (reduce +))))
