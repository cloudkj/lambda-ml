(ns small.data.kd-tree)

;; K-d tree

(defn make-tree
  ([dims points]
   (make-tree dims points 0))
  ([dims points depth]
   (if (empty? points)
     nil
     (let [dim (fn [p] (nth p (mod depth dims)))
           sorted (sort-by dim points)
           median (quot (count sorted) 2)]
       [(nth sorted median)
        (make-tree dims (take median sorted) (inc depth))
        (make-tree dims (drop (+ median 1) sorted) (inc depth))]))))

(defn get-value
  [tree]
  (nth tree 0))

(defn get-left
  [tree]
  (nth tree 1))

(defn get-right
  [tree]
  (nth tree 2))

(defn get-path
  [tree paths]
  (->> paths
       (map (fn [path]
              (cond (= path :left)  1
                    (= path :right) 2
                    :else (throw (IllegalArgumentException. "Invalid tree path")))))
       (get-in tree)))
