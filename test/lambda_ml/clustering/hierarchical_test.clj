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

(deftest test-agglomerative-clustering
  (let [distances {"BA" {"BA" 0   "FI" 662 "MI" 877 "NA" 255 "RM" 412 "TO" 996}
                   "FI" {"BA" 662 "FI" 0   "MI" 295 "NA" 468 "RM" 268 "TO" 400}
                   "MI" {"BA" 877 "FI" 295 "MI" 0   "NA" 754 "RM" 564 "TO" 138}
                   "NA" {"BA" 255 "FI" 468 "MI" 754 "NA" 0   "RM" 219 "TO" 869}
                   "RM" {"BA" 412 "FI" 268 "MI" 564 "NA" 219 "RM" 0   "TO" 669}
                   "TO" {"BA" 996 "FI" 400 "MI" 138 "NA" 869 "RM" 669 "TO" 0}}
        f (fn [a b] (get-in distances [a b]))
        merges (agglomerative-clustering f (keys distances))]
    (is (= [2 5] (nth merges 0)))
    (is (= [3 4] (nth merges 1)))
    (is (= [0 3] (nth merges 2)))
    (is (= [0 1] (nth merges 3)))
    (is (= [0 2] (nth merges 4)))))

(deftest test-agglomerative-clustering2
  (let [points [[1 1 1 0 1 0 0 1 1 1]
                [1 1 0 1 1 0 0 0 0 1]
                [0 1 1 0 1 0 0 1 0 0]
                [0 0 0 1 0 1 0 0 0 0]
                [1 1 1 0 1 0 1 1 1 0]
                [0 1 0 1 1 0 0 0 0 1]
                [0 1 1 0 1 1 0 1 1 0]]
        merges (agglomerative-clustering d/jaccard points)]
    (is (= [1 5] (nth merges 0)))
    (is (= [0 4] (nth merges 1)))
    (is (= [2 6] (nth merges 2)))
    (is (= [0 2] (nth merges 3)))
    (is (= [0 1] (nth merges 4)))
    (is (= [0 3] (nth merges 5)))))

(deftest test-agglomerative-clustering3
  (let [distances {"A" {"A" 0.00 "B" 0.71 "C" 5.66 "D" 3.61 "E" 4.24 "F" 3.20}
                   "B" {"A" 0.71 "B" 0.00 "C" 4.95 "D" 2.92 "E" 3.54 "F" 2.50}
                   "C" {"A" 5.66 "B" 4.95 "C" 0.00 "D" 2.24 "E" 1.41 "F" 2.50}
                   "D" {"A" 3.61 "B" 2.92 "C" 2.24 "D" 0.00 "E" 1.00 "F" 0.50}
                   "E" {"A" 4.24 "B" 3.54 "C" 1.41 "D" 1.00 "E" 0.00 "F" 1.12}
                   "F" {"A" 3.20 "B" 2.50 "C" 2.50 "D" 0.50 "E" 1.12 "F" 0.00}}
        f (fn [a b] (get-in distances [a b]))
        merges (agglomerative-clustering f (keys distances))]
    (is (= [3 5] (nth merges 0)))
    (is (= [0 1] (nth merges 1)))
    (is (= [3 4] (nth merges 2)))
    (is (= [2 3] (nth merges 3)))
    (is (= [0 2] (nth merges 4)))))
