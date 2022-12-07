(ns advent.2022.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [plumbing.core :refer :all]))

(def init-fs {:at []
              :files {}})

(def input
  (-> "resources/2022/day7.input"
      io/reader
      line-seq))

(defn partition-at
  "Like partition-by but will start a new run when f returns true"
  [f coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [run (cons (first s) (take-while #(not (f %)) (rest s)))]
        (cons run (partition-at f (drop (count run) s)))))))

(defn command? [s]
  (str/starts-with? s "$"))

(defn parse-command [lines]
  (let [[_ command arg] (-> lines first (str/split #" "))]
    {:command (keyword command)
     :arg arg
     :output (rest lines)}))

(defn into-fs [fs file-or-dir]
  (let [[word0 filename] (str/split file-or-dir #" ")]
    (if (= word0 "dir")
      (update-in fs
                 (concat [:files] (:at fs) [filename])
                 (fnil identity {}))
      (assoc-in fs
                (concat [:files] (:at fs) [filename])
                (Integer/parseInt word0)))))

(defmulti build-fs (fn [fs {command :command}] command))

(defmethod build-fs :cd [fs {arg :arg}]
  (cond
    (= arg "/")
    (assoc fs :at ["/"])
    (= arg "..")
    (update fs :at (comp vec butlast))
    :else
    (update fs :at conj arg)))

(defmethod build-fs :ls [fs {output :output}]
  (reduce into-fs fs output))

(defn dir? [[_ val]]
  (map? val))

(defn file? [pair]
  (not (dir? pair)))

(defn dir-sizes
  ([all-files] (dir-sizes all-files {} ["/"]))
  ([all-files sizes path]
   (let [files (->> (get-in all-files path) (filter file?))
         subpaths (->> (get-in all-files path)
                       (filter dir?)
                       (map first)
                       (map conj (repeat path)))
         descendant-sizes (apply merge
                                 (mapv (partial dir-sizes all-files sizes)
                                       subpaths))]
     (-> sizes
         (merge descendant-sizes)
         (assoc (str/join "/" path)
                (+ (->> files (map last) (reduce +))
                   (reduce + (map last descendant-sizes))))))))

(defn at-most-100k? [[_ size]]
  (<= size 100000))

(defn run []
  (->> input
       (partition-at command?)
       (map parse-command)
       (reduce build-fs init-fs)
       :files
       dir-sizes
       (filter at-most-100k?)
       (map last)
       (reduce +)))
