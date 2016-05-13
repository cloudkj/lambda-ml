(ns lambda-ml.core-test
  (:require [clojure.test :refer :all]
            [lambda-ml.core :refer :all]))

(deftest test-random-sample
  (doseq [k (range 5 11)]
    (let [s (sample (range 10) k)]
      (is (= k (count s)))
      (is (= k (count (distinct s))))
      (is (every? #(< % 10) s)))))
