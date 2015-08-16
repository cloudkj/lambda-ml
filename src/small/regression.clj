(ns small.regression)

(defn dot-product
  [a b]
  (apply + (map * a b)))

(defn square
  [x]
  (* x x))

;; Linear regression

(defn gradient-descent-step
  "Performs a single gradient step on the model coefficients."
  [x y theta alpha]
  (let [m (count y)
        n+1 (count (first x))
        ;; Compute gradients
        gradients (for [j (range n+1)]
                    (* (/ 1 m)
                       (apply + (map (fn [xi yi]
                                       (* (- (dot-product xi theta) yi)
                                          (xi j)))
                                     x y))))]
    ;; Simultaneously update all thetas
    (map (fn [t g] (- t (* alpha g))) theta gradients)))

(defn gradient-descent
  "Returns an estimate of the model coefficients along with the cost at each
  iteration."
  [x y alpha iters]
  (let [m (count y)
        n+1 (count (first x))]
    (loop [i 0
           theta (repeatedly n+1 rand)
           costs []]
      (if (>= i iters)
        [theta costs]
        (let [theta (gradient-descent-step x y theta alpha)
              cost (/ (apply + (map (fn [xi yi]
                                      (square (- (dot-product xi theta) yi)))
                                    x y))
                      (* 2 m))]
          (recur (inc i) theta (conj costs cost)))))))

(defn make-linear-regression
  "Returns a linear regression model with the given parameters."
  [alpha iters]
  {:alpha alpha
   :iterations iters})

(defn linear-regression-fit
  "Fits a linear regression model to the given training data."
  ([model data]
   (linear-regression-fit model (map butlast data) (map peek data)))
  ([model x y]
   (let [{alpha :alpha iters :iterations} model
         x+intercepts (map (comp vec (partial cons 1)) x)
         [theta costs] (gradient-descent x+intercepts y alpha iters)]
     (-> model
         (assoc :parameters theta)
         (assoc :costs costs)))))

(defn linear-regression-predict
  "Predicts the values of example data using a linear regression model."
  [model x]
  (let [theta (model :parameters)]
    (when (not (nil? theta))
      (->> x
           (map (comp vec (partial cons 1.0)))
           (map (partial dot-product theta))))))
