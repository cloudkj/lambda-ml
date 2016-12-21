(ns lambda-ml.naive-bayes-test
  (:require [clojure.test :refer :all]
            [lambda-ml.naive-bayes :refer :all]))

(deftest test-naive-bayes
  (let [data [[:male   6.00 180 12]
              [:male   5.92 190 11]
              [:male   5.58 170 12]
              [:male   5.92 165 10]
              [:female 5.00 100 6]
              [:female 5.50 150 8]
              [:female 5.42 130 7]
              [:female 5.75 150 9]]
        model (make-naive-bayes)
        fit (naive-bayes-fit model (map #(subvec % 1) data) (map first data))]
    (let [[mean variance] (get-in (:distributions fit) [:male 0])]
      (is (<= (- 5.855 mean) 10E-6))
      (is (<= (- 3.5033E-2 variance) 10E-6)))
    (let [[mean variance] (get-in (:distributions fit) [:female 1])]
      (is (<= (- 132.5 mean) 10E-6))
      (is (<= (- 5.5833E+2 variance) 10E-6)))
    (let [[mean variance] (get-in (:distributions fit) [:male 2])]
      (is (<= (- 11.25 mean) 10E-6))
      (is (<= (- 9.1667E-1 variance) 10E-6)))
    (is (= :female (first (naive-bayes-predict fit [[6.0 130 8]]))))))
