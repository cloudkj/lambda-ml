(ns small.knn
  (:require [small.kdtree :as kd]
            [small.pqueue :as pq]))

;; K-nearest neighbors

(defn euclidean
  "Returns the Euclidean distance between two points. Assumes that both points
  are represented as sequences of the same dimension."
  [a b]
  (->> (map - a b)
       (map #(* % %))
       (reduce +)))

(defn make-knn
  [points]
  "Returns a function that, given k and a query point, returns a priority queue
  of the k nearest neighboring points. Assumes that all points are represented
  as sequences of the same dimension."
  (let [dims (count (first points))
        t (kd/make-tree dims points)]
    (fn knn
      ([k query]
       (knn k query t 0 (pq/make-queue)))
      ([k query tree depth cand]
       (if (nil? tree)
         cand
         (let [[node left right] ((juxt kd/get-value kd/get-left kd/get-right) tree)
               dim (fn [p] (nth p (mod depth dims)))
               ;; Determine near and far branches
               delta (- (dim query) (dim node))
               [near far] (if (<= delta 0) [left right] [right left])
               cand (->>
                     ;; Try to add current node to candidates
                     (pq/insert cand node (euclidean query node) k)
                     ;; Explore near branch
                     (knn k query near (inc depth)))]
           ;; Optionally, explore far branch
           (if (or (< (count cand) k)
                   (< (* delta delta) (pq/item-priority (pq/get-tail cand))))
             (knn k query far (inc depth) cand)
             cand)))))))
