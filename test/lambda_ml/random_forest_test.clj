(ns lambda-ml.random-forest-test
  (:require [clojure.test :refer :all]
            [lambda-ml.random-forest :refer :all]))

(deftest test-random-forest-classifier
  (let [data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]]
        model (make-random-forest-classifier 11 2 1 2)
        fit (random-forest-fit model data)]
    (is (= (first (random-forest-predict fit [[0 0]])) 0))
    (is (= (first (random-forest-predict fit [[0 1]])) 1))
    (is (= (first (random-forest-predict fit [[1 0]])) 1))
    (is (= (first (random-forest-predict fit [[1 1]])) 0))))

(deftest test-random-forest-regressor
  (let [data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]]
        model (make-random-forest-regressor 101 2 1 2)
        fit (random-forest-fit model data)]
    (is (< (first (random-forest-predict fit [[0 0]])) 0.5))
    (is (> (first (random-forest-predict fit [[0 1]])) 0.5))
    (is (> (first (random-forest-predict fit [[1 0]])) 0.5))
    (is (< (first (random-forest-predict fit [[1 1]])) 0.5))))
