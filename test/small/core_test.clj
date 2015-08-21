(ns small.core-test
  (:require [clojure.test :refer :all]
            [small.core :refer :all]))

(deftest test-random-sample
  (doseq [k (range 5 11)]
    (let [sample (random-sample (range 10) k)]
      (is (= k (count sample)))
      (is (= k (count (distinct sample))))
      (is (every? #(< % 10) sample)))))
