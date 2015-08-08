(ns small.knn-test
  (:require [clojure.test :refer :all]
            [small.knn :refer :all]
            [small.pqueue :as pq]))

(deftest test-knn
  (let [knn (make-knn 2 [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= [7 2] (pq/item-value (second (knn 2 [8 1])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [2 3])))))
    (is (= [8 1] (pq/item-value (second (knn 2 [7 2])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [4 7])))))
    (is (= 3 (count (knn 3 [2 3]))))
    (is (= 6 (count (knn 6 [2 3]))))
    (is (= 6 (count (knn 9 [2 3]))))))
