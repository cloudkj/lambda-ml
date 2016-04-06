(ns lambda-ml.regression-test
  (:require [clojure.test :refer :all]
            [lambda-ml.core :refer :all]
            [lambda-ml.regression :refer :all]))

(deftest test-linear-regression
  (let [data [[-2 -1]
              [1 1]
              [3 2]]
        model (make-linear-regression 0.01 0.0 5000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- (/ 5 19) (first coeff))) 1E-6))
    (is (< (Math/abs (- (/ 23 38) (second coeff))) 1E-6))))

(deftest test-linear-regression-regularization
  (let [data (map (fn [[x y]] [x (* x x) (* x x x) (* x x x x) (* x x x x x) y])
                  [[-0.99768 2.0885]
                   [-0.69574 1.1646]
                   [-0.40373 0.3287]
                   [-0.10236 0.46013]
                   [0.22024  0.44808]
                   [0.47742  0.10013]
                   [0.82229  -0.32952]])
        fit-lambda0  (regression-fit (make-linear-regression 0.1 0  10000) data)
        fit-lambda1  (regression-fit (make-linear-regression 0.1 1  10000) data)
        fit-lambda10 (regression-fit (make-linear-regression 0.1 10 10000) data)]
    (is (> (l2-norm (:parameters fit-lambda0))
           (l2-norm (:parameters fit-lambda1))))
    (is (> (l2-norm (:parameters fit-lambda0))
           (l2-norm (:parameters fit-lambda10))))
    (is (> (l2-norm (:parameters fit-lambda1))
           (l2-norm (:parameters fit-lambda10))))))

(deftest test-linear-regression2
  (let [data [[-1 0]
              [0 2]
              [1 4]
              [2 5]]
        model (make-linear-regression 0.01 0.0 5000)
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
        model (make-linear-regression 0.01 0.0 10000)
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
        model (make-logistic-regression 0.1 0 10000)
        {coeff :parameters} (regression-fit model data)]
    (is (< (Math/abs (- -4.077713 (first coeff))) 1E-6))
    (is (< (Math/abs (- 1.504645 (second coeff))) 1E-6))))
