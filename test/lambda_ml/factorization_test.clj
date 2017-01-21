(ns lambda-ml.factorization-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :refer :all]
            [lambda-ml.factorization :refer :all]))

(deftest test-factorization
  (let [data [[1 2 3] [4 5 6]]
        [w h] (-> (factorizations data 2)
                  (nth 200))]
    (is (< (cost data (mmul w h)) 1E-6))))
