(ns lambda-ml.clustering.hierarchical
  "Hierarchical agglomerative clustering."
  (:require [clojure.data.priority-map :as pmap]))

(defn pairwise-distances
  [f points]
  (->> (map-indexed vector points)
       (reduce (fn [distances [i pi]]
                 (->> (map-indexed vector points)
                      (reduce (fn [d [j pj]]
                                (assoc-in d [i j] {:distance (f pi pj) :index j}))
                              distances)))
               {})))

(defn distance-queues
  [distances]
  (->> (keys distances)
       (reduce (fn [queues i]
                 (->> (vals (dissoc (get distances i) i))
                      (reduce (fn [q ci] (assoc q (:index ci) ci))
                              (pmap/priority-map-keyfn :distance))
                      (assoc queues i)))
               {})))

(defn single-link
  [distances x y z]
  (min (get-in distances [x y :distance])
       (get-in distances [x z :distance])))

(defn agglomerative-clustering
  [f points]
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
                                              ;; TODO: specify link function
                                              (let [dist (single-link distances i k1 k2)
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
