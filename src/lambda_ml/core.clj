(ns lambda-ml.core
  (:require [clojure.math.numeric-tower :refer :all]))

(def vector-with-intercept (comp vec (partial cons 1.0)))

(defn dot-product
  [a b]
  (reduce + (map * a b)))

(defn l2-norm
  [a]
  (sqrt (dot-product a a)))

(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))

(defn mode
  [coll]
  (first (apply max-key second (frequencies coll))))

(defn sigmoid
  [z]
  (/ 1 (+ 1 (expt Math/E (- z)))))

(defn random-partition
  "Returns n partitions of elements randomly selected from coll."
  [n coll]
  (let [size (quot (count coll) n)
        coll (shuffle coll)]
    (partition size size [] coll)))

(defn sample-with-replacement
  "Returns n randomly selected elements, with replacement, from coll."
  ([coll n]
   (sample-with-replacement coll n (list)))
  ([coll n s]
   (cond (<= n 0)             s
         (not (vector? coll)) (sample-with-replacement (vec coll) n s)
         :else
         (let [index (rand-int (count coll))]
           (sample-with-replacement coll
                                    (dec n)
                                    (conj s (nth coll index)))))))

(defn sample-without-replacement
  "Returns n randomly selected elements, without replacement, from coll."
  ([coll n]
   (sample-without-replacement coll n (list)))
  ([coll n s]
   (cond (<= n 0)             s
         (empty? coll)        s
         (not (vector? coll)) (sample-without-replacement (vec coll) n s)
         :else
         (let [index (rand-int (count coll))]
           (sample-without-replacement (subvec (assoc coll index (first coll)) 1)
                                       (dec n)
                                       (conj s (nth coll index)))))))
