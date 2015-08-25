(ns lambda-ml.naive-bayes
  (require [clojure.math.numeric-tower :refer :all]))

;; Naive Bayes

(defn gaussian
  [x mean var]
  (* (/ 1 (sqrt (* 2 Math/PI var)))
     (expt Math/E (- (/ (expt (- x mean) 2) (* 2 var))))))

(defn posterior
  [model xi label]
  (if-not (vector? xi)
    (posterior model (vec xi) label)
    (loop [index 0
           prob (/ 1 (count model))]
      (if (>= index (count xi))
        prob
        (let [[mean var] (get-in model [label index])]
          (recur (inc index)
                 (* prob (gaussian (nth xi index) mean var))))))))

(defn naive-bayes-fit
  "Returns a naive Bayes model fit to the given training data."
  ([data]
   (naive-bayes-fit (map butlast data) (map last data)))
  ([x y]
   (cond
     (not-every? vector? x) (naive-bayes-fit (map vec x) y)
     (not (vector? y)) (naive-bayes-fit x (vec y))
     :else
     (let [n (count (first x))]
       (loop [index 0
              labels (distinct y)
              model {}]
         (cond (empty? labels) model
               (>= index n) (recur 0 (rest labels) model)
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
                        (assoc-in model [label index] [mean var])))))))))

(defn naive-bayes-predict
  "Predicts the values of example data using a naive Bayes model."
  [model x]
  (let [labels (keys model)]
    (map (fn [xi] (apply max-key #(posterior model xi %) labels)) x)))
