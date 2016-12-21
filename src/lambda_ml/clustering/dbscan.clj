(ns lambda-ml.clustering.dbscan
  (:require [clojure.set :as set]
            [lambda-ml.data.binary-tree :as bt]
            [lambda-ml.data.kd-tree :as kd]))

(defn make-proximity-search
  "Given a distance function f and a coll of points, returns a function that,
  given a distance and a query point, returns a sequence of all points that are
  within the given distance of the query point."
  [f points]
  (let [dims (count (first points))
        t (kd/make-tree dims points)]
    (fn search
      ([dist query]
       (search dist query t 0 (list)))
      ([dist query tree depth cand]
       (if (nil? tree)
         cand
         (let [[node left right] ((juxt bt/get-value bt/get-left bt/get-right) tree)
               dim (mod depth dims)
               [near far] (if (<= (nth query dim) (nth node dim)) [left right] [right left])]
           (cond->> cand
             ;; Add current node if it's within proximity
             (<= (f query node) dist)
             (cons node)
             ;; Explore near branch
             true
             (search dist query near (inc depth))
             ;; Optionally, explore far branch
             (< (f query node dim) dist)
             (search dist query far (inc depth)))))))))

(defn dbscan
  "Returns a clustering of points represented as a map from cluster id to a set
  of points, using the epsilon parameter for neighborhood lookups and forming
  clusters with at least min-pts density."
  [f epsilon min-pts points]
  (let [search (make-proximity-search f points)]
    (loop [unvisited points
           cluster-id 0
           visited #{}
           clusters {}]
      (let [point (first unvisited)]
        (cond
          ;; No more points
          (nil? point)
          clusters
          ;; Already visited
          (visited point)
          (recur (rest unvisited) cluster-id visited clusters)
          ;; Visit point
          :else
          (let [visited (conj visited point)
                neighbors (search epsilon point)]
            (if (< (count neighbors) min-pts)
              ;; Noise
              (recur (rest unvisited) cluster-id visited clusters)
              ;; Expand cluster
              (let [cluster-id (+ 1 cluster-id)
                    ;; Assign point to cluster
                    clusters (assoc clusters point cluster-id)
                    ;; Find all neighbors-of-neighbors
                    expanded (reduce (fn [n i]
                                       (if (visited i)
                                         (conj n i)
                                         (let [nn (search epsilon i)]
                                           (if (< (count nn) min-pts)
                                             (conj n i)
                                             (set/union n (set nn))))))
                                     #{}
                                     neighbors)]
                (recur (rest unvisited)
                       cluster-id
                       ;; Mark expanded neighbors as visited
                       (reduce conj visited expanded)
                       ;; Assign expanded neighbors to clusters
                       (reduce (fn [c i] (if (c i) c (assoc c i cluster-id)))
                               clusters
                               expanded))))))))))
