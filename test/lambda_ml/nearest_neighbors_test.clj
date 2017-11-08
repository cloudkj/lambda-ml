(ns lambda-ml.nearest-neighbors-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [lambda-ml.nearest-neighbors :refer :all]
            [lambda-ml.distance :as d]))

(deftest test-nearest-neighbor-search
  (let [search (make-nearest-neighbor-search d/euclidean [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= [7 2] (item-value (second (search 2 [8 1])))))
    (is (= [5 4] (item-value (second (search 2 [2 3])))))
    (is (= [8 1] (item-value (second (search 2 [7 2])))))
    (is (= [5 4] (item-value (second (search 2 [4 7])))))
    (is (= 3 (count (search 3 [2 3]))))
    (is (= 6 (count (search 6 [2 3]))))
    (is (= 6 (count (search 9 [2 3]))))))

(deftest test-nearest-neighbor-search2
  (let [search (make-nearest-neighbor-search d/euclidean [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= [4 8] (item-value (first (search 5 [3 9])))))))

(deftest test-nearest-neighbor-search3
  (let [points {[0.0   0.0]    1
                [10.1  -10.1]  2
                [-12.2 12.2]   3
                [38.3  38.3]   4
                [79.99 179.99] 5}
        search (make-nearest-neighbor-search d/euclidean (keys points))]
    (is (= (list 1 2 3 4)
           (map (comp points second) (search 4 ((map-invert points) 1)))))
    (is (= (list 2 1 3 4)
           (map (comp points second) (search 4 ((map-invert points) 2)))))
    (is (= (list 3 1 2 4)
           (map (comp points second) (search 4 ((map-invert points) 3)))))
    (is (= (list 4 1 2 3)
           (map (comp points second) (search 4 ((map-invert points) 4)))))
    (is (= (list 5 4 3 1)
           (map (comp points second) (search 4 ((map-invert points) 5)))))))

(deftest test-nearest-neighbor-search4
  (let [points {[0.436697697345292 0.492281587956396] 1
                [0.318000697283004 0.302602867518914] 2
                [0.268674100320323 0.684132163547525] 3
                [0.347190228888873 0.959920716313895] 4
                [0.539212291014011 0.187100169547265] 5
                [0.964631186098456 0.129079314315528] 6
                [0.171792010609788 0.795749621321345] 7
                [0.910157297130659 0.437962722965556] 8
                [0.847975159955406 0.169625495659256] 9
                [0.793504465072615 0.121750314432942] 10}
        search (make-nearest-neighbor-search d/euclidean (keys points))]
    (is (= (list 1 2 3 5)
           (map (comp points second) (search 4 ((map-invert points) 1)))))
    (is (= (list 2 1 5 3)
           (map (comp points second) (search 4 ((map-invert points) 2)))))
    (is (= (list 3 7 1 4)
           (map (comp points second) (search 4 ((map-invert points) 3)))))
    (is (= (list 4 7 3 1)
           (map (comp points second) (search 4 ((map-invert points) 4)))))
    (is (= (list 5 2 10 9)
           (map (comp points second) (search 4 ((map-invert points) 5)))))
    (is (= (list 6 9 10 8)
           (map (comp points second) (search 4 ((map-invert points) 6)))))
    (is (= (list 7 3 4 1)
           (map (comp points second) (search 4 ((map-invert points) 7)))))
    (is (= (list 8 9 6 10)
           (map (comp points second) (search 4 ((map-invert points) 8)))))
    (is (= (list 9 10 6 8)
           (map (comp points second) (search 4 ((map-invert points) 9)))))
    (is (= (list 10 9 6 5)
           (map (comp points second) (search 4 ((map-invert points) 10)))))))

(deftest test-nearest-neighbor-search5
  (let [points {[36.971838 -122.019653] :SantaCruz,
                [37.864012 -122.277832] :Berkeley,
                [37.330857 -121.887817] :SanJose,
                [37.444335 -122.156982] :PaloAlto,
                [37.387617 -122.060852] :MountainView,
                [37.759859 -122.437134] :SanFrancisco}
        search (make-nearest-neighbor-search d/euclidean (keys points))]
    (is (= :SanJose
           (-> (search 2 ((map-invert points) :SantaCruz)) second item-value points)))
    (is (= :SanFrancisco
           (-> (search 2 ((map-invert points) :Berkeley)) second item-value points)))
    (is (= :MountainView
           (-> (search 2 ((map-invert points) :PaloAlto)) second item-value points)))))

(deftest test-nearest-neighbor-search-metadata
  (let [points [[:a 2 3]
                [:b 5 4]
                [:c 9 6]
                [:d 4 7]
                [:e 8 1]
                [:f 7 2]]
        search (make-nearest-neighbor-search d/euclidean rest points)]
    (is (= :f (first (item-value (second (search 2 [:e 8 1]))))))
    (is (= :b (first (item-value (second (search 2 [:a 2 3]))))))
    (is (= :e (first (item-value (second (search 2 [:f 7 2]))))))
    (is (= :b (first (item-value (second (search 2 [:d 4 7]))))))))

(deftest test-nearest-neighbors-classifier
  (let [data [[25 40000  :no]
              [35 60000  :no]
              [45 80000  :no]
              [20 20000  :no]
              [35 120000 :no]
              [52 18000  :no]
              [23 95000  :yes]
              [40 62000  :yes]
              [60 100000 :yes]
              [48 220000 :yes]
              [33 150000 :yes]]
        fit1 (-> (make-nearest-neighbors-classifier 1 d/euclidean)
                 (nearest-neighbors-fit data))
        fit3 (-> (make-nearest-neighbors-classifier 3 d/euclidean)
                 (nearest-neighbors-fit data))]
    (is (= (first (nearest-neighbors-predict fit1 [[48 142000]])) :yes))
    (is (= (first (nearest-neighbors-predict fit3 [[48 142000]])) :yes))))

(deftest test-nearest-neighbors-regressor
  (let [data [[25 40000  135]
              [35 60000  256]
              [45 80000  231]
              [20 20000  267]
              [35 120000 139]
              [52 18000  150]
              [23 95000  127]
              [40 62000  216]
              [60 100000 139]
              [48 220000 250]
              [33 150000 264]]
        fit1 (-> (make-nearest-neighbors-regressor 1 d/euclidean)
                 (nearest-neighbors-fit data))
        fit3 (-> (make-nearest-neighbors-regressor 3 d/euclidean)
                 (nearest-neighbors-fit data))]
    (is (= (first (nearest-neighbors-predict fit1 [[48 142000]])) 264))
    (is (= (first (nearest-neighbors-predict fit3 [[48 142000]])) (/ 542 3)))))
