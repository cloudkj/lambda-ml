(ns lambda-ml.random-forest
  "Random forest classification and regression learning.

  Example usage:
  ```
  (def data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]])
  (def fit
    (let [n 1001
          min-split 2
          min-leaf 1
          max-features 2]
      (-> (make-random-forest-classifier n min-split min-leaf max-features)
          (random-forest-fit data))))
  (random-forest-predict fit (map butlast data))
  ;;=> (0 1 1 0)
  ```"
  (:require [lambda-ml.decision-tree :as dt]
            [lambda-ml.ensemble :as e]))

(def random-forest-fit e/bagging-ensemble-fit)

(def random-forest-predict e/bagging-ensemble-predict)

(defn make-random-forest-classifier
  [n min-split min-leaf max-features]
  (let [rate 1.0
        estimator (dt/make-classification-tree dt/gini-impurity min-split min-leaf max-features)]
    (-> #(e/add-bagging-estimator % estimator dt/decision-tree-fit dt/decision-tree-predict)
        (iterate (e/make-bagging-classifier rate))
        (nth n))))

(defn make-random-forest-regressor
  [n min-split min-leaf max-features]
  (let [rate 1.0
        estimator (dt/make-regression-tree dt/mean-squared-error min-split min-leaf max-features)]
    (-> #(e/add-bagging-estimator % estimator dt/decision-tree-fit dt/decision-tree-predict)
        (iterate (e/make-bagging-regressor rate))
        (nth n))))
