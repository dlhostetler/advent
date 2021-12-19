(ns advent.2021.day19
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(defn parse-coords [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn parse-scan [s]
  (->> s
       str/split-lines
       (drop 1)
       (map parse-coords)
       (into #{})))

(def scans
  (->> (-> "resources/2021/day19.input"
           io/reader
           slurp
           (str/split #"\n\n"))
       (mapv parse-scan)))

(defn negative [x]
  (* -1 x))

(defn coordnientations [[a b c]]
  [[a, b, c]
   [b, c, a]
   [c, a, b]
   [c, b, (negative a)]
   [b, a, (negative c)]
   [a, c, (negative b)]

   [a, (negative b), (negative c)]
   [b, (negative c), (negative a)]
   [c, (negative a), (negative b)]
   [c, (negative b), a]
   [b, (negative a), c]
   [a, (negative c), b]

   [(negative a), b, (negative c)]
   [(negative b), c, (negative a)]
   [(negative c), a, (negative b)]
   [(negative c), b, a]
   [(negative b), a, c]
   [(negative a), c, b]

   [(negative a), (negative b), c]
   [(negative b), (negative c), a]
   [(negative c), (negative a), b]
   [(negative c), (negative b), (negative a)]
   [(negative b), (negative a), (negative c)]
   [(negative a), (negative c), (negative b)]])

(alter-var-root #'coordnientations memoize)

(defn transpose [m]
  (apply mapv vector m))

(defn orientations [scan]
  (transpose (map coordnientations scan)))

(alter-var-root #'orientations memoize)

(defn coords-diff [coords-a coords-b]
  (mapv - coords-a coords-b))

(defn adjust-coords [coords-a diff]
  (mapv + coords-a diff))

(defn find-commons [scan-a scan-b diff]
  (set/intersection scan-a
                    (set (map adjust-coords scan-b (repeat diff)))))

(defn find-translation [scan-a scan-b]
  (-> (for [coords-a scan-a
            coords-b scan-b
            :let [translation (coords-diff coords-a coords-b)
                  commons (find-commons scan-a scan-b translation)]
            :when (>= (count commons) 12)]
        translation)
      first))

(defn find-match [scan-a scan-b]
  (->> (for [b-orientation (orientations scan-b)
             :let [translation (find-translation scan-a b-orientation)]
             :when translation]
         {:orig-scan scan-b
          :scanner-at translation
          :translated-scan (set (map adjust-coords
                                     b-orientation
                                     (repeat translation)))})
       first))

(defn match-next [{:keys [remaining translated]}]
  (->> (for [scan-a remaining
             scan-b (map :translated-scan translated)
             :let [match (find-match scan-b scan-a)]
             :when match]
         {:remaining (remove #(= % scan-a) remaining)
          :translated (conj translated match)})
       first))

(defn translate-scanners [scans]
  (loop [state {:remaining (rest scans)
                :translated [{:orig-scan (first scans)
                              :scanner-at [0 0 0]
                              :translated-scan (first scans)}]}]
    (println (-> state :remaining count) "Remaining")
    (if (pos? (-> state :remaining count))
      (recur (match-next state))
      (:translated state))))

(defn run []
  (->> scans
       translate-scanners
       (mapcat :translated-scan)
       set
       count))
