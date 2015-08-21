(ns small.clustering
  (:require [clojure.set :as set]
            [small.core :as c]
            [small.distance :as d]
            [small.neighborhood :as n]))

;; DBSCAN

(defn dbscan
  "Returns a clustering of points represented as a map from cluster id to a set
  of points, using the epsilon parameter for neighborhood lookups and forming
  clusters with at least min-pts density."
  [f epsilon min-pts points]
  (let [search (n/make-search f points)]
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

;; K-means

(defn assign-clusters
  "Returns cluster assignments based on the closest centroid to each point."
  [mu x]
  (let [mu-indexed (map-indexed vector mu)]
    (loop [points x
           clusters {}]
      (if (empty? points)
        clusters
        (let [xi (first points)
              ;; Find the index of the closest centroid
              index (first (apply min-key (comp (partial d/euclidean xi) second) mu-indexed))
              cluster (or (clusters index) (list))]
          (recur (rest points)
                 (assoc clusters index (conj cluster xi))))))))

(defn update-centroids
  "Returns updated centroids based on the average of points in each cluster."
  [k clusters]
  (map (fn [index]
         (->> (clusters index)
              (apply map +)
              (map #(/ % (count (clusters index))))))
       (range k)))

(defn k-means
  "Returns a clustering of points represented as a map from cluster id to a
  collection of points."
  [k points iters]
  (loop [i 0
         mu (c/random-sample points k)
         clusters {}]
    (if (>= i iters)
      clusters
      (let [clusters (assign-clusters mu points)]
        (recur (inc i)
               (update-centroids k clusters)
               clusters)))))
