(ns advent.2020.day23)

(def total-cups 1000000)

(def cups (->> "523764819"
               seq
               (map str)
               (mapv #(Integer/parseInt %))))

(defn link [cups [cup0 cup1]]
  (assoc cups cup0 cup1))

(defn linkify
  "Create a map (really a vector) of cup values to the value of the next cup."
  [cups]
  (let [cup->cup (vec (repeat (-> cups count inc) nil))]
    ;; each cup points to another cup
    (-> (reduce link cup->cup (partition 2 1 cups))
        (assoc
          ;; the last cup points at the first cup
          (last cups)
          (first cups)
          ;; the first index points at the first cup (there is no cup zero)
          0
          (first cups)))))

(defn find-destination [current invalid]
  (loop [destination (dec current)]
    (if (contains? invalid destination)
      (recur (mod (dec destination) (inc total-cups)))
      destination)))

(defn move [cup->cup _]
  (let [current (cup->cup 0)
        grabbed0 (cup->cup current)
        grabbed1 (cup->cup grabbed0)
        grabbed2 (cup->cup grabbed1)
        after-grabbed (cup->cup grabbed2)
        destination (find-destination current #{grabbed0
                                                grabbed1
                                                grabbed2
                                                0})
        after-destination (cup->cup destination)]
    (assoc cup->cup
      ;; point index 0 (current) to the cup after the grabbed cups
      0 after-grabbed
      ;; point the current cup to the cup after the grabbed cups
      current after-grabbed
      ;; point the destination cup at the first grabbed cup
      destination grabbed0
      ;; point the last grabbed cup at the cup after the destination
      grabbed2 after-destination)))

(defn play [n cups]
  (reduce move cups (range n)))

(defn answer [cup->cup]
  (->> [(cup->cup 1) (cup->cup (cup->cup 1))]
       (reduce *)))

(defn run []
  (->> (concat cups (range (-> cups count inc)
                           (inc total-cups)))
       linkify
       (play 10000000)
       answer))
