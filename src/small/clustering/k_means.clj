(ns small.clustering.k-means
  (:require [small.core :as c]
            [small.distance :as d]))

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
