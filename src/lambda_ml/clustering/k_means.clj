(ns lambda-ml.clustering.k-means
  (:require [lambda-ml.core :as c]
            [lambda-ml.distance :as d]))

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

(defn k-means-seq
  [k points centroids]
  (lazy-seq (let [clusters (assign-clusters centroids points)]
              (cons clusters
                    (k-means-seq k points (update-centroids k clusters))))))

(defn k-means
  "Returns a lazy sequence of clustering of points represented as a map from
  cluster id to a collection of points, at each iteration of k-means."
  [k points]
  (k-means-seq k points (c/random-sample points k)))
