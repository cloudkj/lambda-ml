(ns lambda-ml.neural-network-test
  (:require [clojure.test :refer :all]
            [lambda-ml.neural-network :refer :all]))

(deftest test-feed-forward
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        x [0.05 0.1]
        [hidden output] (feed-forward x weights)]
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
        x [0.6 0.1]
        [hidden output] (feed-forward x weights)]
    (is (< (Math/abs (- 0.53494294 (nth hidden 0))) 1E-6))
    (is (< (Math/abs (- 0.55477923 (nth hidden 1))) 1E-6))
    (is (< (Math/abs (- 0.65475346 (nth hidden 2))) 1E-6))
    (is (< (Math/abs (- 0.53353777 (nth output 0))) 1E-6))
    (is (< (Math/abs (- 0.62727869 (nth output 1))) 1E-6))))
