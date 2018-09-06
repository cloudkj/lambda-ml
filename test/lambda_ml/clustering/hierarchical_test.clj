(ns lambda-ml.clustering.hierarchical-test
  (:require [clojure.test :refer :all]
            ;; TODO: remove
            [clojure.pprint :refer :all]
            [lambda-ml.clustering.hierarchical :refer :all]
            [lambda-ml.distance :as d]))

(deftest test-pairwise-distances
  (let [points [[1 1 1 0 1 0 0 1 1 1]
                [1 1 0 1 1 0 0 0 0 1]
                [0 1 1 0 1 0 0 1 0 0]
                [0 0 0 1 0 1 0 0 0 0]
                [1 1 1 0 1 0 1 1 1 0]
                [0 1 0 1 1 0 0 0 0 1]
                [0 1 1 0 1 1 0 1 1 0]]
        distances (pairwise-distances d/jaccard points)]
    (is (= 0 (:distance (get-in distances [0 0]))))
    (is (= (/ 1 2) (:distance (get-in distances [0 1]))))
    (is (= (/ 3 7) (:distance (get-in distances [0 2]))))
    (is (= 1 (:distance (get-in distances [0 3]))))
    (is (= (/ 1 4) (:distance (get-in distances [0 4]))))
    (is (= (/ 5 8) (:distance (get-in distances [0 5]))))
    (is (= (/ 3 8) (:distance (get-in distances [0 6]))))
    (is (= (/ 1 2) (:distance (get-in distances [1 0]))))
    (is (= 0 (:distance (get-in distances [1 1]))))
    (is (= (/ 5 7) (:distance (get-in distances [1 2]))))
    (is (= (/ 5 6) (:distance (get-in distances [1 3]))))
    (is (= (/ 2 3) (:distance (get-in distances [1 4]))))
    (is (= (/ 1 5) (:distance (get-in distances [1 5]))))
    (is (= (/ 7 9) (:distance (get-in distances [1 6]))))
    (is (= (/ 3 7) (:distance (get-in distances [2 0]))))
    (is (= (/ 5 7) (:distance (get-in distances [2 1]))))
    (is (= 0 (:distance (get-in distances [2 2]))))
    (is (= 1 (:distance (get-in distances [2 3]))))
    (is (= (/ 3 7) (:distance (get-in distances [2 4]))))
    (is (= (/ 2 3) (:distance (get-in distances [2 5]))))
    (is (= (/ 1 3) (:distance (get-in distances [2 6]))))
    (is (= 1 (:distance (get-in distances [3 0]))))
    (is (= (/ 5 6) (:distance (get-in distances [3 1]))))
    (is (= 1 (:distance (get-in distances [3 2]))))
    (is (= 0 (:distance (get-in distances [3 3]))))
    (is (= 1 (:distance (get-in distances [3 4]))))
    (is (= (/ 4 5) (:distance (get-in distances [3 5]))))
    (is (= (/ 6 7) (:distance (get-in distances [3 6]))))
    (is (= (/ 1 4) (:distance (get-in distances [4 0]))))
    (is (= (/ 2 3) (:distance (get-in distances [4 1]))))
    (is (= (/ 3 7) (:distance (get-in distances [4 2]))))
    (is (= 1 (:distance (get-in distances [4 3]))))
    (is (= 0 (:distance (get-in distances [4 4]))))
    (is (= (/ 7 9) (:distance (get-in distances [4 5]))))
    (is (= (/ 3 8) (:distance (get-in distances [4 6]))))
    (is (= (/ 5 8) (:distance (get-in distances [5 0]))))
    (is (= (/ 1 5) (:distance (get-in distances [5 1]))))
    (is (= (/ 2 3) (:distance (get-in distances [5 2]))))
    (is (= (/ 4 5) (:distance (get-in distances [5 3]))))
    (is (= (/ 7 9) (:distance (get-in distances [5 4]))))
    (is (= 0 (:distance (get-in distances [5 5]))))
    (is (= (/ 3 4) (:distance (get-in distances [5 6]))))
    (is (= (/ 3 8) (:distance (get-in distances [6 0]))))
    (is (= (/ 7 9) (:distance (get-in distances [6 1]))))
    (is (= (/ 1 3) (:distance (get-in distances [6 2]))))
    (is (= (/ 6 7) (:distance (get-in distances [6 3]))))
    (is (= (/ 3 8) (:distance (get-in distances [6 4]))))
    (is (= (/ 3 4) (:distance (get-in distances [6 5]))))
    (is (= 0 (:distance (get-in distances [6 6]))))))

(deftest test-distance-queues
  (let [distances {0 {0 {:distance 0 :index 0}
                      1 {:distance 7 :index 1}
                      2 {:distance 3 :index 2}}
                   1 {0 {:distance 7 :index 0}
                      1 {:distance 0 :index 1}
                      2 {:distance 5 :index 2}}
                   2 {0 {:distance 3 :index 0}
                      1 {:distance 5 :index 1}
                      2 {:distance 0 :index 2}}}
        queues (distance-queues distances)]
    (is (= 2 (first (first (get queues 0)))))
    (is (= 1 (first (second (get queues 0)))))
    (is (= 2 (first (first (get queues 1)))))
    (is (= 0 (first (second (get queues 1)))))
    (is (= 0 (first (first (get queues 2)))))
    (is (= 1 (first (second (get queues 2)))))))
