(ns small.core)

(defn random-sample
  "Returns n randomly selected elements, without replacement, from coll."
  ([coll n]
   (random-sample coll n (list)))
  ([coll n sample]
   (if-not (vector? coll)
     (random-sample (vec coll) n sample)
     (let [index (rand-int (count coll))]
       (if (or (<= n 0) (empty? coll))
         sample
         (random-sample (subvec (assoc coll index (first coll)) 1)
                        (dec n)
                        (conj sample (nth coll index))))))))
