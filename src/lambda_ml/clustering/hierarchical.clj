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
