(ns lambda-ml.clustering.hierarchical
  "Hierarchical agglomerative clustering.

  Example usage:
  ```
  (def data [[1 1 1 0 1 0 0 1 1 1]
             [1 1 0 1 1 0 0 0 0 1]
             [0 1 1 0 1 0 0 1 0 0]
             [0 0 0 1 0 1 0 0 0 0]
             [1 1 1 0 1 0 1 1 1 0]
             [0 1 0 1 1 0 0 0 0 1]
             [0 1 1 0 1 1 0 1 1 0]])
  (agglomerative-clustering single-link lambda-ml.distance/euclidean data)
  ;;=> [[1 5] [0 4] [2 6] [0 2] [0 1] [0 3]]
  ```"
  (:require [clojure.data.priority-map :as pmap]))

(defn pairwise-distances
  "Returns a map representing the distance matrix between all points."
  [f points]
  (->> (map-indexed vector points)
       (reduce (fn [distances [i pi]]
                 (->> (map-indexed vector points)
                      (reduce (fn [d [j pj]]
                                (assoc-in d [i j] {:distance (f pi pj) :index j}))
                              distances)))
               {})))

(defn distance-queues
  "Returns a map of each point to a priority queue of all other points sorted by
  increasing distance."
  [distances]
  (->> (keys distances)
       (reduce (fn [queues i]
                 (->> (vals (dissoc (get distances i) i))
                      (reduce (fn [q ci] (assoc q (:index ci) ci))
                              (pmap/priority-map-keyfn :distance))
                      (assoc queues i)))
               {})))

(defn single-link
  "Returns the single-link distance between point x and the merged cluster
  containing points y and z, which is the distance between x and the closest
  point in the cluster."
  [distances x y z]
  (min (get-in distances [x y :distance])
       (get-in distances [x z :distance])))

(defn complete-link
  "Returns the complete-link distance between point x and the merged cluster
  containing points y and z, which is the distance between x and the farthest
  point in the cluster."
  [distances x y z]
  (max (get-in distances [x y :distance])
       (get-in distances [x z :distance])))

(defn agglomerative-clustering
  "Returns a clustering of points represented as a seq of merges, where each
  merge is a pair of indexes indicating the two points to be merged at each
  step, using the linkage function link and distance function f."
  [link f points]
  (loop [distances (pairwise-distances f points)
         queues (distance-queues distances)
         active (reduce #(assoc %1 %2 true) {} (range (count points)))
         merges []]
    (if (<= (count active) 1)
      merges
      (let [;; Find the two most similar clusters
            [_ k1 k2] (->> (keys active)
                           (reduce (fn [[min-dist k1 k2] i]
                                     (let [[k {dist :distance}] (peek (get queues i))]
                                       (if (< dist min-dist)
                                         [dist i k]
                                         [min-dist k1 k2])))
                                   [Double/MAX_VALUE nil nil]))
            ;; Clear queue for k1
            queues (assoc queues k1 (pmap/priority-map-keyfn :distance))
            ;; Update distances
            [distances queues] (->> (keys active)
                                    (filter #(and (not (= k1 %)) (not (= k2 %))))
                                    (reduce (fn [[d q] i]
                                              (let [dist (link distances i k1 k2)
                                                    d (-> (assoc-in d [i k1 :distance] dist)
                                                          (assoc-in [k1 i :distance] dist))
                                                    q (-> (update q i #(dissoc % k1))
                                                          (update i #(dissoc % k2))
                                                          (update i #(assoc % k1 (get-in d [i k1])))
                                                          (update k1 #(assoc % i (get-in d [k1 i]))))]
                                                [d q]))
                                            [distances queues]))]
        (recur distances
               queues
               (dissoc active k2)         ;; Deactivate cluster k2
               (conj merges [k1 k2])))))) ;; Merge k1 and k2
