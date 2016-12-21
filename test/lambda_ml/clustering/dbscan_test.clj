(ns lambda-ml.clustering.dbscan-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [lambda-ml.clustering.dbscan :refer :all]
            [lambda-ml.distance :as d]))

(deftest test-proximity-search
  (let [points {:SanFrancisco [37.759859 -122.437134]
                :Berkeley     [37.864012 -122.277832]
                :PaloAlto     [37.444335 -122.156982]
                :MountainView [37.387617 -122.060852]
                :SanJose      [37.330857 -121.887817]
                :SantaCruz    [36.971838 -122.019653]}
        locations (map-invert points)
        search (make-proximity-search d/haversine (vals points))]
    (is (= (set (map locations (search 1 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 2 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 4 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 8 (points :MountainView))))
           #{:MountainView :PaloAlto}))
    (is (= (set (map locations (search 16 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose}))
    (is (= (set (map locations (search 30 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz}))
    (is (= (set (map locations (search 35 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco}))
    (is (= (set (map locations (search 50 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco :Berkeley}))))

(deftest test-dbscan
  (let [points [[2 10]
                [2 5]
                [8 4]
                [5 8]
                [7 5]
                [6 4]
                [1 2]
                [4 9]]]
    (let [clustering (dbscan d/euclidean 4 2 points)]
      (is (= 2 (count (distinct (vals clustering)))))
      (is (= (clustering [5 8])
             (clustering [4 9])))
      (is (= (clustering [8 4])
             (clustering [7 5])
             (clustering [6 4]))))
    (let [clustering (dbscan d/euclidean 10 2 points)]
      (is (= 3 (count (distinct (vals clustering)))))
      (is (= (clustering [2 10])
             (clustering [5 8])
             (clustering [4 9])))
      (is (= (clustering [8 4])
             (clustering [7 5])
             (clustering [6 4])))
      (is (= (clustering [2 5])
             (clustering [1 2]))))))

(deftest test-dbscan2
  (let [points [[0 100]
                [0 200]
                [0 275]
                [100 150]
                [200 100]
                [250 200]
                [0 300]
                [100 200]
                [600 700]
                [650 700]
                [675 700]
                [675 710]
                [675 720]
                [50 400]]
        clustering (dbscan d/euclidean 10000 3 points)]
    (is (= 2 (count (distinct (vals clustering)))))
    (is (= (clustering [0 100])
           (clustering [0 200])
           (clustering [0 275])
           (clustering [100 150])
           (clustering [0 300])
           (clustering [100 200])))
    (is (= (clustering [600 700])
           (clustering [650 700])
           (clustering [675 700])
           (clustering [675 710])
           (clustering [675 720])))))

(deftest test-dbscan3
  (let [points [[64.22906466107816  21.979356040013954]
                [9.502019068226218  73.5146190142259]
                [73.467643359676    49.11882050731219]
                [43.89991499437019  30.877086140215397]
                [31.66112900408916  62.006799353519455]
                [32.96188162290491  62.647924402495846]
                [29.860327935311943 61.4603339463938]
                [30.252436050213873 61.628358017420396]
                [29.478669344214723 63.34734829352237]
                [31.20809231721796  60.62778950878419]
                [56.719979556510225 12.79888943536207]
                [58.65873904968612  12.760463243583217]
                [56.961131111718714 13.99614970618859]
                [56.45650068600662  14.442072300706059]
                [56.4971734713763   15.955536322668195]
                [56.84485225030221  14.559824497206137]
                [55.51035064924508  11.432733331574127]
                [56.13687033439055  10.726739906473085]
                [55.76618270971812  11.259738458553329]
                [18.786322022089735 39.41377491992898]
                [17.121476165484637 39.838845615973696]
                [19.683527131564915 39.34956996375695]]
        clustering (dbscan d/euclidean 4 2 points)]
    (is (= (clustering (nth points 4))
           (clustering (nth points 5))
           (clustering (nth points 6))
           (clustering (nth points 7))
           (clustering (nth points 8))
           (clustering (nth points 9))))
    (is (= (clustering (nth points 10))
           (clustering (nth points 11))
           (clustering (nth points 12))
           (clustering (nth points 13))
           (clustering (nth points 14))
           (clustering (nth points 15))
           (clustering (nth points 16))
           (clustering (nth points 17))
           (clustering (nth points 18))))
    (is (= (clustering (nth points 19))
           (clustering (nth points 20))
           (clustering (nth points 21))))))
