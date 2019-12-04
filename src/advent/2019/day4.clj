(ns advent.2019.day4)

(defn strict-adjacent-pair? [v]
  (->> v
       (partition-by identity)
       (some #(= 2 (count %)))))

(defn vaguely-increasing? [[a b]]
  (<= a b))

(defn password? [^String s]
  (let [v (->> s seq (map str) (map #(Integer/parseInt %)))]
    (and (= 6 (count s))
         (strict-adjacent-pair? v)
         (every? vaguely-increasing? (partition 2 1 v)))))

(defn run []
  (->> (range 240298 784957)
       (map str)
       (filter password?)
       count))