(ns small.knn-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [small.knn :refer :all]
            [small.kdtree :as kd]
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

(deftest test-search
  (let [points {:SanFrancisco [37.759859 -122.437134]
                :Berkeley     [37.864012 -122.277832]
                :PaloAlto     [37.444335 -122.156982]
                :MountainView [37.387617 -122.060852]
                :SanJose      [37.330857 -121.887817]
                :SantaCruz    [36.971838 -122.019653]}
        locations (map-invert points)
        tree (kd/make-tree 2 (vals points))]
    (is (= (set (map locations (search 1 (points :MountainView) tree)))
           #{:MountainView}))
    (is (= (set (map locations (search 2 (points :MountainView) tree)))
           #{:MountainView}))
    (is (= (set (map locations (search 4 (points :MountainView) tree)))
           #{:MountainView}))
    (is (= (set (map locations (search 8 (points :MountainView) tree)))
           #{:MountainView :PaloAlto}))
    (is (= (set (map locations (search 16 (points :MountainView) tree)))
           #{:MountainView :PaloAlto :SanJose}))
    (is (= (set (map locations (search 30 (points :MountainView) tree)))
           #{:MountainView :PaloAlto :SanJose :SantaCruz}))
    (is (= (set (map locations (search 35 (points :MountainView) tree)))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco}))
    (is (= (set (map locations (search 50 (points :MountainView) tree)))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco :Berkeley}))))
