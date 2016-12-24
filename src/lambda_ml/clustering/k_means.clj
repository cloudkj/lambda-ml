(ns lambda-ml.clustering.k-means
  "K-means clustering.

  Example usage:
  ```
  (def data [[1 1] [1.5 2] [3 4] [5 7] [3.5 5] [4.5 5] [3.5 4.5]])
  (let [k 2]
    (-> (k-means k lambda-ml.distance/euclidean data)
        (nth 100)))
  ;;=> {0 ([3.5 4.5] [4.5 5] [3.5 5] [5 7] [3 4]), 1 ([1.5 2] [1 1])}
  ```"
  (:require [lambda-ml.core :as c]))

(defn assign-clusters
  "Returns cluster assignments based on the closest centroid to each point."
  [f mu x]
  (let [mu-indexed (map-indexed vector mu)]
    (loop [points x
           clusters {}]
      (if (empty? points)
        clusters
        (let [xi (first points)
              ;; Find the index of the closest centroid
              index (first (apply min-key (comp (partial f xi) second) mu-indexed))
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
  [k f points centroids]
  (lazy-seq (let [clusters (assign-clusters f centroids points)]
              (cons clusters
                    (k-means-seq k f points (update-centroids k clusters))))))

(defn k-means
  "Returns a lazy sequence of a clustering of points using the distance function
  f, represented as a map from cluster id to a collection of points, at each
  iteration of k-means."
  [k f points]
  (k-means-seq k f points (c/sample-without-replacement points k)))
