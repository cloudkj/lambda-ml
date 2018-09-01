(ns lambda-ml.distance-test
  (:require [clojure.test :refer :all]
            [lambda-ml.distance :refer :all]))

(deftest test-cosine
  (is (< (Math/abs (- (cosine [1 2 0] [0 4 1])
                      0.132278))
         1E-6)
  (is (< (Math/abs (- (cosine [0 3 4 5] [7 6 3 1])
                      0.492167))
         1E-6))))

(deftest test-euclidean
  (is (= 25 (euclidean [2 -1]    [-2 2])))
  (is (= 95 (euclidean [0 3 4 5] [7 6 3 -1]))))

(deftest test-haversine
  (is (< (Math/abs (- (haversine [36.12 -86.67] [33.94 -118.40])
                      1794.0717860923137))
         1E-6)))

(deftest test-haversine2
  (is (< (Math/abs (- (haversine [36.12 -86.67] [33.94 -118.40] 0)
                      150.66697884839715))
         1E-6))
  (is (< (Math/abs (- (haversine [36.12 -86.67] [33.94 -118.40] 1)
                      2192.964788467725))
         1E-6)))

(deftest test-jaccard
  (is (= (/ 3 5) (jaccard [1 1 0 1] [2 0 1 1])))
  (is (= (/ 3 7) (jaccard [1 1 1 0 1 0 0 1 1 1] [0 1 1 0 1 0 0 1 0 0])))
  (is (= (/ 5 7) (jaccard [1 1 0 1 1 0 0 0 0 1] [0 1 1 0 1 0 0 1 0 0])))
  (is (= (/ 3 7) (jaccard [0 1 1 0 1 0 0 1 0 0] [1 1 1 0 1 0 1 1 1 0])))
  (is (= (/ 6 7) (jaccard [0 0 0 1 0 1 0 0 0 0] [0 1 1 0 1 1 0 1 1 0]))))
