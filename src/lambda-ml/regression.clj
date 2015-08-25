(ns lambda-ml.regression
  (require [clojure.math.numeric-tower :refer :all]))

(defn dot-product
  [a b]
  (apply + (map * a b)))

(defn sigmoid
  [z]
  (/ 1 (+ 1 (expt Math/E (- z)))))

(def vector-with-intercept (comp vec (partial cons 1.0)))

(defn gradient-descent-step
  "Performs a single gradient step on the model coefficients."
  [h x y theta alpha]
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
    (map (fn [t g] (- t (* alpha g))) theta gradients)))

(defn gradient-descent
  "Returns an estimate of the model coefficients along with the cost at each
  iteration. Takes a hypothesis function h, which returns a predicted value
  given an example and parameters, and a cost function j, which computes the
  cost of applying the current model on all training examples."
  [h j x y alpha iters]
  (let [m (count y)
        n+1 (count (first x))]
    (loop [i 0
           theta (repeatedly n+1 rand)
           costs []]
      (if (>= i iters)
        [theta costs]
        (let [theta (gradient-descent-step h x y theta alpha)
              cost (j x y theta)]
          (recur (inc i) theta (conj costs cost)))))))

(defn regression-fit
  "Fits a regression model to the given training data."
  ([model data]
   (regression-fit model (map butlast data) (map last data)))
  ([model x y]
   (let [{alpha :alpha iters :iterations h :hypothesis j :cost} model
         x+intercepts (map vector-with-intercept x)
         [theta costs] (gradient-descent h j x+intercepts y alpha iters)]
     (-> model
         (assoc :parameters theta)
         (assoc :costs costs)))))

(defn regression-predict
  "Predicts the values of example data using a regression model."
  [model x]
  (let [{theta :parameters h :hypothesis} model]
    (when (not (nil? theta))
      (->> x
           (map vector-with-intercept)
           (map (partial h theta))))))

;; Linear regression

(defn linear-regression-hypothesis
  [xi theta]
  (dot-product xi theta))

(defn linear-regression-cost
  [x y theta]
  (let [m (count y)]
    (/ (apply + (map (fn [xi yi]
                       (expt (- (linear-regression-hypothesis xi theta) yi) 2))
                     x y))
       (* 2 m))))

(defn make-linear-regression
  "Returns a linear regression model with the given parameters."
  [alpha iters]
  {:alpha alpha
   :iterations iters
   :hypothesis linear-regression-hypothesis
   :cost linear-regression-cost})

;; Logistic regression

(defn logistic-regression-hypothesis
  [xi theta]
  (sigmoid (dot-product xi theta)))

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
  [alpha iters]
  {:alpha alpha
   :iterations iters
   :hypothesis logistic-regression-hypothesis
   :cost logistic-regression-cost})
