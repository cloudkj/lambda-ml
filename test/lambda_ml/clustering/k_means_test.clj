(ns lambda-ml.clustering.k-means-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [lambda-ml.clustering.k-means :refer :all]
            [lambda-ml.distance :as d]))

(deftest test-k-means
  (let [points [[1 1] [1.5 2] [3 4] [5 7] [3.5 5] [4.5 5] [3.5 4.5]]]
    (let [clustering (nth (k-means 2 d/euclidean points) 100)
          index (map-invert clustering)]
      (is (= 2 (count clustering)))
      (is (= (index [3.5 4.5])
             (index [4.5 5])
             (index [3.5 5])
             (index [5 7])
             (index [3 4]))
          (= (index [1.5 2])
             (index [1 1]))))))
