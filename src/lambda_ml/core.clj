(ns lambda-ml.core
  (:require [clojure.math.numeric-tower :refer :all]))

(defn dot-product
  [a b]
  (reduce + (map * a b)))

(defn sigmoid
  [z]
  (/ 1 (+ 1 (expt Math/E (- z)))))

(defn random-partition
  "Returns n partitions of elements randomly selected from coll."
  [n coll]
  (let [size (quot (count coll) n)
        coll (shuffle coll)]
    (partition size size [] coll)))

(defn random-sample
  "Returns n randomly selected elements, without replacement, from coll."
  ([coll n]
   (random-sample coll n (list)))
  ([coll n sample]
   (if-not (vector? coll)
     (random-sample (vec coll) n sample)
     (let [index (rand-int (count coll))]
       (if (or (<= n 0) (empty? coll))
         sample
         (random-sample (subvec (assoc coll index (first coll)) 1)
                        (dec n)
                        (conj sample (nth coll index))))))))
