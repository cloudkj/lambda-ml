(ns lambda-ml.regression
  "Generalized linear model learning for two of the more popular techniques,
  linear regression and logistic regression.

  Linear regression example usage:
  ```
  (def data [[-2 -1] [1 1] [3 2]])
  (def fit
  (let [alpha 0.01
        lambda 0
        iters 5000]
    (-> (make-linear-regression alpha lambda iters)
        (regression-fit data))))
  (regression-predict fit (map butlast data))
  ;;=> (-0.9473684210526243 0.8684210526315812 2.0789473684210513)
  ```

  Logistic regression example usage:
  ```
  (def data [[4.0 1] [1.75 0] [4.25 1] [2.75 1] [5.0 1] [0.5 0] [1.0 0] [1.5 0]
             [5.5 1] [2.5 0] [2.0 0] [3.5 0] [1.75 1] [3.0 0] [4.75 1] [1.25 0]
             [4.5 1] [0.75 0] [3.25 1] [2.25 1]])
  (def fit
    (let [alpha 0.1
          lambda 0
          iters 10000]
      (-> (make-logistic-regression alpha lambda iters)
          (regression-fit data))))
  (regression-predict fit (take 3 (map butlast data)))
  ;;=> (0.8744474608195764 0.19083657134699333 0.9102776017566352)
  ```"
  (:require [lambda-ml.core :as c]
            [clojure.math.numeric-tower :refer :all]))

(defn gradient-descent-step
  "Performs a single gradient step on the model coefficients."
  [h x y alpha lambda theta]
  (let [m (count y)
        n+1 (count (first x))
        ;; Compute gradients
        gradients (for [j (range n+1)]
                    (* (/ 1 m)
                       (apply + (map (fn [xi yi]
                                       (* (- (h xi theta) yi)
                                          (xi j)))
                                     x y))))]
    ;; Simultaneously update all thetas
    (map-indexed (fn [i [t g]]
                   (if (= i 0)
                     ;; Non-regularized intercept parameter
                     (- t (* alpha g))
                     ;; Regularized parameters
                     (- (* t (- 1 (/ (* alpha lambda) m)))
                        (* alpha g))))
                 (map vector theta gradients))))

(defn gradient-descent
  "Returns a lazy sequence of estimates of the model coefficients, along with
  the cost, at each iteration of gradient descent. Takes a hypothesis function
  h, which returns a predicted value given an example and parameters, and a cost
  function j, which computes the cost of applying the current model on all
  training examples."
  ([h j x y alpha lambda]
   (let [n+1 (count (first x))]
     (gradient-descent h j x y alpha lambda (repeatedly n+1 rand))))
  ([h j x y alpha lambda theta]
   (lazy-seq (let [theta (gradient-descent-step h x y alpha lambda theta)
                   cost (j x y theta)]
               (cons [theta cost] (gradient-descent h j x y alpha lambda theta))))))

(defn regression-fit
  "Fits a regression model to the given training data."
  ([model data]
   (regression-fit model (map butlast data) (map last data)))
  ([model x y]
   (let [{alpha :alpha lambda :lambda iters :iterations h :hypothesis j :cost} model
         x+intercepts (map c/vector-with-intercept x)
         estimates (gradient-descent h j x+intercepts y alpha lambda)
         [theta cost] (nth estimates iters)]
     (-> model
         (assoc :parameters theta)
         (assoc :costs (map second (take iters estimates)))))))

(defn regression-predict
  "Predicts the values of example data using a regression model."
  [model x]
  (let [{theta :parameters h :hypothesis} model]
    (when (not (nil? theta))
      (->> x
           (map c/vector-with-intercept)
           (map (partial h theta))))))

;; Linear regression

(defn linear-regression-hypothesis
  [xi theta]
  (c/dot-product xi theta))

(defn linear-regression-cost
  [x y theta]
  (let [m (count y)]
    (/ (apply + (map (fn [xi yi]
                       (expt (- (linear-regression-hypothesis xi theta) yi) 2))
                     x y))
       (* 2 m))))

(defn make-linear-regression
  "Returns a linear regression model with the given parameters."
  [alpha lambda iters]
  {:alpha alpha
   :lambda lambda
   :iterations iters
   :hypothesis linear-regression-hypothesis
   :cost linear-regression-cost})

;; Logistic regression

(defn logistic-regression-hypothesis
  [xi theta]
  (c/sigmoid (c/dot-product xi theta)))

(defn logistic-regression-cost
  [x y theta]
  (let [m (count y)]
    (/ (apply + (map (fn [xi yi]
                       (let [hi (logistic-regression-hypothesis xi theta)]
                         (+ (* yi
                               (Math/log hi))
                            (* (- 1 yi)
                               (Math/log (- 1 hi))))))
                     x y))
       (- m))))

(defn make-logistic-regression
  "Returns a logistic regression model with the given parameters."
  [alpha lambda iters]
  {:alpha alpha
   :lambda lambda
   :iterations iters
   :hypothesis logistic-regression-hypothesis
   :cost logistic-regression-cost})
