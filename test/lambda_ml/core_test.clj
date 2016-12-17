(ns lambda-ml.core-test
  (:require [clojure.test :refer :all]
            [lambda-ml.core :refer :all]))

(deftest test-median
  (is (= (median [5 2 4 1 3]) 3))
  (is (= (median [7 0 2 3]) (/ 5 2))))

(deftest test-sample-with-replacement
  (doseq [k (range 5 11)]
    (let [s (sample-with-replacement (range 10) k)]
      (is (= k (count s)))
      (is (every? #(< % 10) s)))))

(deftest test-sample-without-replacement
  (is (= 10 (count (sample-without-replacement (range 10) 10))))
  (is (= 10 (count (sample-without-replacement (range 10) 100))))
  (is (= 10 (count (sample-without-replacement (range 10) 1000))))
  (doseq [k (range 5 11)]
    (let [s (sample-without-replacement (range 10) k)]
      (is (= k (count s)))
      (is (= k (count (distinct s))))
      (is (every? #(< % 10) s)))))
