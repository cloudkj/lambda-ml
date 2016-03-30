(ns lambda-ml.metrics)

(defn roc-curve
  "Returns a sequence of [false positive rate, true positive rate] tuples that
  represent the ROC curve of a classifier."
  [labels predictions]
  (let [p (reduce + (filter (fn [x] (= x 1)) labels))
        n (- (count labels) p)
        ranked (->> (map vector labels predictions)
                    (sort-by second)
                    (map first)
                    (reverse))]
    (loop [ys ranked
           fp 0
           tp 0
           points []]
      (if (empty? ys)
        points
        (let [fp (if (= (first ys) 0) (inc fp) fp)
              tp (if (= (first ys) 1) (inc tp) tp)
              fpr (/ fp n)
              tpr (/ tp p)]
          (recur (rest ys) fp tp (conj points [fpr tpr])))))))
