(ns small.regression-test
  (:require [clojure.test :refer :all]
            [small.regression :refer :all]))

(deftest test-linear-regression
  (let [data [[-2 -1]
              [1 1]
              [3 2]]
        model (make-linear-regression 0.01 5000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- (/ 5 19) (first coeff))) 1E-6))
    (is (< (Math/abs (- (/ 23 38) (second coeff))) 1E-6))))

(deftest test-linear-regression2
  (let [data [[-1 0]
              [0 2]
              [1 4]
              [2 5]]
        model (make-linear-regression 0.01 5000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- 1.9 (first coeff))) 1E-6))
    (is (< (Math/abs (- 1.7 (second coeff))) 1E-6))))

(deftest test-linear-regression3
  (let [data [[4 390]
              [9 580]
              [10 650]
              [14 730]
              [4 410]
              [7 530]
              [12 600]
              [22 790]
              [1 350]
              [3 400]
              [8 590]
              [11 640]
              [5 450]
              [6 520]
              [10 690]
              [11 690]
              [16 770]
              [13 700]
              [13 730]
              [10 640]]
        model (make-linear-regression 0.01 10000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- 353.16487949889 (first coeff))) 1E-6))
    (is (< (Math/abs (- 25.326467777896 (second coeff))) 1E-6))))

(deftest test-logistic-regression
  (let [data [[0.50 0]
              [0.75 0]
              [1.00 0]
              [1.25 0]
              [1.50 0]
              [1.75 0]
              [1.75 1]
              [2.00 0]
              [2.25 1]
              [2.50 0]
              [2.75 1]
              [3.00 0]
              [3.25 1]
              [3.50 0]
              [4.00 1]
              [4.25 1]
              [4.50 1]
              [4.75 1]
              [5.00 1]
              [5.50 1]]
        model (make-logistic-regression 0.1 10000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- -4.077713 (first coeff))) 1E-6))
    (is (< (Math/abs (- 1.504645 (second coeff))) 1E-6))))
