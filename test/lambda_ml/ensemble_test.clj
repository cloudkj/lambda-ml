(ns lambda-ml.ensemble-test
  (:require [clojure.test :refer :all]
            [lambda-ml.ensemble :refer :all]
            [lambda-ml.decision-tree :refer :all]))

(deftest test-bagging-classifier
  (let [data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]]
        tree (make-classification-tree gini-impurity 2 1 (dec (count (first data))))
        model (-> (iterate #(add-bagging-estimator % tree decision-tree-fit decision-tree-predict)
                            (make-bagging-classifier 1.0))
                  (nth 1013))
        fit (bagging-ensemble-fit model data)]
    (is (= (first (bagging-ensemble-predict fit [[0 0]])) 0))
    (is (= (first (bagging-ensemble-predict fit [[0 1]])) 1))
    (is (= (first (bagging-ensemble-predict fit [[1 0]])) 1))
    (is (= (first (bagging-ensemble-predict fit [[1 1]])) 0))))

(deftest test-bagging-regressor
  (let [data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]]
        tree (make-regression-tree mean-squared-error 2 1 (dec (count (first data))))
        model (-> (iterate #(add-bagging-estimator % tree decision-tree-fit decision-tree-predict)
                            (make-bagging-regressor 1.0))
                  (nth 1003))
        fit (bagging-ensemble-fit model data)]
    (is (< (first (bagging-ensemble-predict fit [[0 0]])) 0.5))
    (is (> (first (bagging-ensemble-predict fit [[0 1]])) 0.5))
    (is (> (first (bagging-ensemble-predict fit [[1 0]])) 0.5))
    (is (< (first (bagging-ensemble-predict fit [[1 1]])) 0.5))))
