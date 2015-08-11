(ns small.knn
  (:require [small.distance :as d]
            [small.kdtree :as kd]
            [small.pqueue :as pq]))

;; K-nearest neighbors

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
               dim (mod depth dims)
               ;; Determine near and far branches
               [near far] (if (<= (nth query dim) (nth node dim)) [left right] [right left])
               cand (->>
                     ;; Try to add current node to candidates
                     (pq/insert cand node (d/euclidean query node) k)
                     ;; Explore near branch
                     (knn k query near (inc depth)))]
           ;; Optionally, explore far branch
           (if (or (< (count cand) k)
                   (< (d/euclidean query node dim) (pq/item-priority (pq/get-tail cand))))
             (knn k query far (inc depth) cand)
             cand)))))))

;; Proximity search

(defn search
  "Returns a sequence of all points in the tree that are within a given distance
  of the query point."
  ([dist query tree]
   (search dist query tree 0 (list)))
  ([dist query tree depth cand]
   (if (nil? tree)
     cand
     (let [[node left right] ((juxt kd/get-value kd/get-left kd/get-right) tree)
           dim (mod depth (count node))
           [near far] (if (<= (nth query dim) (nth node dim)) [left right] [right left])]
       (cond->> cand
         ;; Add current node if it's within proximity
         (< (d/haversine query node) dist)
         (cons node)
         ;; Explore near branch
         true
         (search dist query near (inc depth))
         ;; Optionally, explore far branch
         (< (d/haversine query node dim) dist)
         (search dist query far (inc depth)))))))
