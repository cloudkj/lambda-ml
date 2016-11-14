(ns lambda-ml.metrics-test
  (:require [clojure.test :refer :all]
            [lambda-ml.metrics :refer :all]))

(deftest test-auc
  (is (= 0.75 (auc [[0 0.5] [0.5 0.5] [0.5 1] [1 1]])))
  (is (= 147.66 (auc [[0 100] [1 50] [2 25] [3 12.5] [4 6.25] [5 3.13] [6 1.56]]))))

(deftest test-gini-impurity
  (is (< (Math/abs (- (gini-impurity [:b :b :b :b :b :b]) 0)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :b :b :b :b :b]) 0.277778)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :a :a :b :b :b]) 0.5)) 1E-6)))

(deftest test-mean-squared-error
  (is (= 0.375 (mean-squared-error [3 -0.5 2 7] [2.5 0.0 2 8]))))
