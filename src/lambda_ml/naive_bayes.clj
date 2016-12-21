(ns lambda-ml.naive-bayes
  (require [clojure.math.numeric-tower :refer :all]))

;; Naive Bayes

(defn gaussian
  [x mean var]
  (* (/ 1 (sqrt (* 2 Math/PI var)))
     (expt Math/E (- (/ (expt (- x mean) 2) (* 2 var))))))

(defn posterior
  [distributions xi label]
  (if-not (vector? xi)
    (posterior distributions (vec xi) label)
    (loop [index 0
           prob (/ 1 (count distributions))]
      (if (>= index (count xi))
        prob
        (let [[mean var] (get-in distributions [label index])]
          (recur (inc index)
                 (* prob (gaussian (nth xi index) mean var))))))))

(defn naive-bayes-fit
  "Returns a naive Bayes model fit to the given training data."
  ([model data]
   (naive-bayes-fit model (map butlast data) (map last data)))
  ([model x y]
   (cond
     (not-every? vector? x) (naive-bayes-fit model (map vec x) y)
     (not (vector? y))      (naive-bayes-fit model x (vec y))
     :else
     (let [n (count (first x))]
       (loop [index 0
              labels (distinct y)
              m {}]
         (cond (empty? labels) (assoc model :distributions m)
               (>= index n)    (recur 0 (rest labels) m)
               :else
               (let [label (first labels)
                     ;; Feature values for examples with the current label
                     vals (->> (map #(nth % index) x)
                               (keep-indexed (fn [i xi] (when (= (nth y i) label) xi))))
                     mean (/ (apply + vals) (count vals))
                     ;; Unbiased sample variance
                     var (/ (apply + (map #(expt (- % mean) 2) vals)) (dec (count vals)))]
                 (recur (inc index)
                        labels
                        (assoc-in m [label index] [mean var])))))))))

(defn naive-bayes-predict
  "Predicts the values of example data using a naive Bayes model."
  [model x]
  (let [{distributions :distributions} model
        labels (keys distributions)]
    (map (fn [xi] (apply max-key #(posterior distributions xi %) labels)) x)))

(defn make-naive-bayes
  "Returns a naive Bayes model."
  []
  {})
