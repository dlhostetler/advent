(ns advent.2019.intcode-test
  (:require [clojure.test :refer :all]
            [advent.2019.intcode :as intcode]))

(deftest t-execute-instructions-0
  (is (= [2 0 0 0 99]
         (intcode/execute-instructions [1 0 0 0 99]))))

(deftest t-execute-instructions-1
  (is (= [2 3 0 6 99]
         (intcode/execute-instructions [2 3 0 3 99]))))

(deftest t-execute-instructions-2
  (is (= [2 4 4 5 99 9801]
         (intcode/execute-instructions [2 4 4 5 99 0]))))

(deftest t-execute-instructions-3
  (is (= [30 1 1 4 2 5 6 0 99]
         (intcode/execute-instructions [1 1 1 4 99 5 6 0 99]))))
