(ns advent.2019.intcode-test
  (:require [advent.2019.intcode :as intcode]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(deftest t-add-positional
  (is (= [1 0 0 5 99 2]
         (intcode/execute-instructions :test [1 0 0 5 99]))))

(deftest t-add-immediate
  (is (= [1101 1 1 5 99 2]
         (intcode/execute-instructions :test [1101 1 1 5 99]))))

(deftest t-multiply-positional
  (is (= [2 0 0 5 99 4]
         (intcode/execute-instructions :test [2 0 0 5 99]))))

(deftest t-multiply-immediate
  (is (= [2 2 2 5 99 4]
         (intcode/execute-instructions :test [2 2 2 5 99]))))

(deftest t-input-fn
  (is (= [3 3 99 42]
         (intcode/execute-instructions :test [3 3 99] (constantly 42) nil nil))))

(deftest t-input-stdin
  (is (= [3 3 99 42]
         (with-in-str "42\n"
                      (intcode/execute-instructions :test [3 3 99] intcode/stdin nil nil)))))

(deftest t-input-chan
  (let [chan (async/chan 1)]
    (async/>!! chan 42)
    (is (= [3 3 99 42]
           (intcode/execute-instructions :test [3 3 99] (intcode/chan->in chan) nil nil)))))

(deftest t-output-fn
  (let [out (atom nil)]
    (intcode/execute-instructions :test [4 3 99 42] nil #(reset! out %) nil)
    (is (= 42 @out))))

(deftest t-output-stdout
  (is (= "42"
         (-> (intcode/execute-instructions :test [4 3 99 42] nil intcode/stdout nil)
             with-out-str
             str/trim))))

(deftest t-output-chan
  (let [chan (async/chan 1)]
    (intcode/execute-instructions :test [4 3 99 42] nil (intcode/chan->out chan) nil)
    (is (= 42 (async/<!! chan)))))

(deftest t-jump-if-true-true
  ;; will jump to halt and skip the add
  (is (= [1105 1 7 1101 1 1 8 99]
         (intcode/execute-instructions :test [1105 1 7 1101 1 1 8 99]))))

(deftest t-jump-if-true-false
  ;; will not jump and execute the add
  (is (= [1105 0 7 1101 1 1 8 99 2]
         (intcode/execute-instructions :test [1105 0 7 1101 1 1 8 99]))))

(deftest t-jump-if-false-true
  ;; will not jump and execute the add
  (is (= [1106 1 7 1101 1 1 8 99 2]
         (intcode/execute-instructions :test [1106 1 7 1101 1 1 8 99]))))

(deftest t-jump-if-false-false
  ;; will jump to halt and skip the add
  (is (= [1106 0 7 1101 1 1 8 99]
         (intcode/execute-instructions :test [1106 0 7 1101 1 1 8 99]))))

(deftest t-less-than-true
  (is (= [1107 0 1 5 99 1]
         (intcode/execute-instructions :test [1107 0 1 5 99]))))

(deftest t-less-than-false
  (is (= [1107 1 0 5 99 0]
         (intcode/execute-instructions :test [1107 1 0 5 99]))))

(deftest t-equals-true
  (is (= [1108 0 0 5 99 1]
         (intcode/execute-instructions :test [1108 0 0 5 99]))))

(deftest t-equals-false
  (is (= [1108 0 1 5 99 0]
         (intcode/execute-instructions :test [1108 0 1 5 99]))))

(deftest t-relative-base-offset
  (is (= [109 2 1201 1 1 7 99 2]
         (intcode/execute-instructions :test [109 2 1201 1 1 7 99]))))

(deftest t-halt
  (let [halted (atom false)]
    (intcode/execute-instructions :test [99] nil nil #(reset! halted true))
    (is @halted)))
