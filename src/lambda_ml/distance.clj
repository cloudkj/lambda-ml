(ns lambda-ml.distance
  "Functions that compute measures of distance between values."
  (:require [lambda-ml.core :as c]))

(defn cosine
  "Returns the cosine distance between two points by subtracting the cosine
  similarity from one. Assumes that both points are represented as sequences of
  the same dimension. Given a dimension d, returns the distance between two
  points as if the values for all other dimensions were set to zero."
  ([x y]
   (- 1 (/ (c/dot-product x y)
           (* (Math/sqrt (c/dot-product x x))
              (Math/sqrt (c/dot-product y y))))))
  ([x y d]
   (cosine (vector (nth x d)) (vector (nth y d)))))

(defn euclidean
  "Returns the Euclidean distance (squared) between two points. Assumes that
  both points are represented as sequences of the same dimension. Given a
  dimension d, returns the distance between two points as if the values for all
  other dimensions were set to zero."
  ([x y]
   (->> (map - x y)
        (map #(* % %))
        (reduce +)))
  ([x y d]
   (euclidean (vector (nth x d)) (vector (nth y d)))))

(defn haversine
  "Returns the great-circle distance between two points represented as
  geographic coordinates. Given a dimension d, returns the distance between the
  two points as if the value for the other dimension was set to zero."
  ([[lat1 lng1] [lat2 lng2]]
   (let [r 3959.9 ; miles; km = 6372.8
         dlat (Math/toRadians (- lat2 lat1))
         dlng (Math/toRadians (- lng2 lng1))
         lat1 (Math/toRadians lat1)
         lat2 (Math/toRadians lat2)
         a (+ (* (Math/sin (/ dlat 2)) (Math/sin (/ dlat 2)))
              (* (Math/sin (/ dlng 2)) (Math/sin (/ dlng 2)) (Math/cos lat1) (Math/cos lat2)))]
     (* r 2 (Math/asin (Math/sqrt a)))))
  ([x y d]
   (let [other (mod (inc d) 2)]
   (haversine (assoc x other 0) (assoc y other 0)))))
