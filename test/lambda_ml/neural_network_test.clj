(ns lambda-ml.neural-network-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as m]
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

(deftest test-back-propagate
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        fs' [sigmoid' sigmoid']
        y [0.01 0.99]
        activations [[0.593269920 0.596884378] [0.751365070 0.772928465]]
        [errors1 errors2] (back-propagate y weights fs' activations quadratic-output-error)]
    (is (< (Math/abs (- 0.00877136 (first errors1)))   1E-6))
    (is (< (Math/abs (- 0.00995425 (second errors1)))  1E-6))
    (is (< (Math/abs (- 0.13849856 (first errors2)))   1E-6))
    (is (< (Math/abs (- -0.03809824 (second errors2))) 1E-6))))

(deftest test-compute-gradients
  (let [x [0.05 0.1]
        activations [[0.593269920 0.596884378] [0.751365070 0.772928465]]
        errors [[0.00877136 0.00995425] [0.13849856 -0.03809824]]
        [g0 g1] (compute-gradients x activations errors)]
    (is (< (Math/abs (- 0.00877136 (nth (nth g0 0) 0))) 1E-6))
    (is (< (Math/abs (- 0.00043857 (nth (nth g0 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.00087713 (nth (nth g0 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.00995425 (nth (nth g0 1) 0))) 1E-6))
    (is (< (Math/abs (- 0.00049771 (nth (nth g0 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.00099543 (nth (nth g0 1) 2))) 1E-6))
    (is (< (Math/abs (- 0.13849856 (nth (nth g1 0) 0))) 1E-6))
    (is (< (Math/abs (- 0.08216703 (nth (nth g1 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.08266763 (nth (nth g1 0) 2))) 1E-6))
    (is (< (Math/abs (- -0.03809824 (nth (nth g1 1) 0))) 1E-6))
    (is (< (Math/abs (- -0.02260254 (nth (nth g1 1) 1))) 1E-6))
    (is (< (Math/abs (- -0.02274024 (nth (nth g1 1) 2))) 1E-6))))

(deftest test-regularize
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        alpha 0.5
        lambda 0.1
        [r0 r1] (regularize weights alpha lambda)]
    (is (< (Math/abs (- 0.0 (nth (nth r0 0) 0))) 1E-6))
    (is (< (Math/abs (- 0.0075 (nth (nth r0 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.01 (nth (nth r0 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.0 (nth (nth r0 1) 0))) 1E-6))
    (is (< (Math/abs (- 0.0125 (nth (nth r0 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.015 (nth (nth r0 1) 2))) 1E-6))
    (is (< (Math/abs (- 0.0 (nth (nth r1 0) 0))) 1E-6))
    (is (< (Math/abs (- 0.02 (nth (nth r1 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.0225 (nth (nth r1 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.0 (nth (nth r1 1) 0))) 1E-6))
    (is (< (Math/abs (- 0.025 (nth (nth r1 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.0275 (nth (nth r1 1) 2))) 1E-6))))

(deftest test-gradient-descent-step
  (let [weights [[[0.35 0.15 0.20]
                  [0.35 0.25 0.30]]
                 [[0.60 0.40 0.45]
                  [0.60 0.50 0.55]]]
        model (-> (make-neural-network 0.5 0 quadratic-cost)
                  (add-neural-network-layer 2 sigmoid)
                  (add-neural-network-layer 2 sigmoid)
                  (add-neural-network-layer 2 sigmoid))
        fs [sigmoid sigmoid]
        x [0.05 0.1]
        y [0.01 0.99]
        [w0 w1] (gradient-descent-step model x y weights)]
    (is (< (Math/abs (- 0.149780716 (nth (nth w0 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.19956143  (nth (nth w0 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.24975114  (nth (nth w0 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.29950229  (nth (nth w0 1) 2))) 1E-6))
    (is (< (Math/abs (- 0.35891648  (nth (nth w1 0) 1))) 1E-6))
    (is (< (Math/abs (- 0.408666186 (nth (nth w1 0) 2))) 1E-6))
    (is (< (Math/abs (- 0.51130127  (nth (nth w1 1) 1))) 1E-6))
    (is (< (Math/abs (- 0.561370121 (nth (nth w1 1) 2))) 1E-6))))

(deftest test-init-parameters
  (let [model {:layers [2 3 1] :seed 12345}
        [w0 w1] (init-parameters model)]
    (is (= [3 3] (m/shape w0)))
    (is (= [1 4] (m/shape w1)))
    (is (< (Math/abs (- -0.18780898 (m/mget w0 0 0))) 1E-6))
    (is (< (Math/abs (- 0.58843630 (m/mget w0 0 1))) 1E-6))
    (is (< (Math/abs (- 0.94880478 (m/mget w0 0 2))) 1E-6))
    (is (< (Math/abs (- -0.49428072 (m/mget w0 1 0))) 1E-6))
    (is (< (Math/abs (- -1.22341193 (m/mget w0 1 1))) 1E-6))
    (is (< (Math/abs (- -0.69796098 (m/mget w0 1 2))) 1E-6))
    (is (< (Math/abs (- -0.77722490 (m/mget w0 2 0))) 1E-6))
    (is (< (Math/abs (- 2.06800870 (m/mget w0 2 1))) 1E-6))
    (is (< (Math/abs (- -0.58734674 (m/mget w0 2 2))) 1E-6))
    (is (< (Math/abs (- 0.46214534 (m/mget w1 0 0))) 1E-6))
    (is (< (Math/abs (- 1.37458180 (m/mget w1 0 1))) 1E-6))
    (is (< (Math/abs (- -0.09785321 (m/mget w1 0 2))) 1E-6))
    (is (< (Math/abs (- -1.07643638 (m/mget w1 0 3))) 1E-6))))

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
