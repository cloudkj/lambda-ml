(ns lambda-ml.neural-network-test
  (:require [clojure.test :refer :all]
            [lambda-ml.core :refer :all]
            [lambda-ml.neural-network :refer :all]))

(deftest test-feed-forward
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        fs [sigmoid sigmoid]
        x [0.05 0.1]
        [hidden output] (feed-forward x weights fs)]
    (is (< (Math/abs (- 0.593269920 (first hidden)))  1E-6))
    (is (< (Math/abs (- 0.596884378 (second hidden))) 1E-6))
    (is (< (Math/abs (- 0.751365070 (first output)))  1E-6))
    (is (< (Math/abs (- 0.772928465 (second output))) 1E-6))))

(deftest test-feed-forward2
  (let [weights [[[ 0.1   0.1  -0.2]
                  [ 0.2   0     0.2]
                  [ 0.5   0.3  -0.4]]
                 [[-0.1  -0.4   0.1   0.6]
                  [ 0.6   0.2  -0.1  -0.2]]]
        fs [sigmoid sigmoid]
        x [0.6 0.1]
        [hidden output] (feed-forward x weights fs)]
    (is (< (Math/abs (- 0.53494294 (nth hidden 0))) 1E-6))
    (is (< (Math/abs (- 0.55477923 (nth hidden 1))) 1E-6))
    (is (< (Math/abs (- 0.65475346 (nth hidden 2))) 1E-6))
    (is (< (Math/abs (- 0.53353777 (nth output 0))) 1E-6))
    (is (< (Math/abs (- 0.62727869 (nth output 1))) 1E-6))))

(deftest test-gradient-descent-step
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        fs [sigmoid sigmoid]
        x [0.05 0.1]
        y [0.01 0.99]
        alpha 0.5
        lambda 0
        [w0 w1] (gradient-descent-step x y weights fs
                                       alpha lambda
                                       quadratic-cost
                                       quadratic-output-error)]
    (is (< (Math/abs (- 0.149780716 (nth (nth w0 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.19956143  (nth (nth w0 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.24975114  (nth (nth w0 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.29950229  (nth (nth w0 1) 2))) 1E-6))
    (is (< (Math/abs (- 0.35891648  (nth (nth w1 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.408666186 (nth (nth w1 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.51130127  (nth (nth w1 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.561370121 (nth (nth w1 1) 2))) 1E-6))))

(deftest test-neural-network
  (let [data [[0 0 [0]]
              [0 1 [1]]
              [1 0 [1]]
              [1 1 [0]]]
        model (-> (make-neural-network 0.5 0.0 cross-entropy-cost 54321)
                  (add-neural-network-layer 2 sigmoid)
                  (add-neural-network-layer 3 sigmoid)
                  (add-neural-network-layer 1 sigmoid))
        fit (nth (iterate #(neural-network-fit % data) model) 5000)
        predictions (map first (neural-network-predict fit (map butlast data)))]
    (is (> 0.1 (nth predictions 0)))
    (is (< 0.9 (nth predictions 1)))
    (is (< 0.9 (nth predictions 2)))
    (is (> 0.1 (nth predictions 3)))))
