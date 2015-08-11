(ns small.knn-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [small.knn :refer :all]
            [small.pqueue :as pq]))

(deftest test-knn
  (let [knn (make-knn [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= [7 2] (pq/item-value (second (knn 2 [8 1])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [2 3])))))
    (is (= [8 1] (pq/item-value (second (knn 2 [7 2])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [4 7])))))
    (is (= 3 (count (knn 3 [2 3]))))
    (is (= 6 (count (knn 6 [2 3]))))
    (is (= 6 (count (knn 9 [2 3]))))))

(deftest test-knn2
  (let [knn (make-knn [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= [4 8] (pq/item-value (first (knn 5 [3 9])))))))

(deftest test-knn3
  (let [points {[36.971838 -122.019653] :SantaCruz,
                [37.864012 -122.277832] :Berkeley,
                [37.330857 -121.887817] :SanJose,
                [37.444335 -122.156982] :PaloAlto,
                [37.387617 -122.060852] :MountainView,
                [37.759859 -122.437134] :SanFrancisco}
        knn (make-knn (keys points))]
    (is (= :SanJose
           (-> (knn 2 ((map-invert points) :SantaCruz)) second pq/item-value points)))
    (is (= :SanFrancisco
           (-> (knn 2 ((map-invert points) :Berkeley)) second pq/item-value points)))
    (is (= :MountainView
           (-> (knn 2 ((map-invert points) :PaloAlto)) second pq/item-value points)))))
