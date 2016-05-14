(ns lambda-ml.neural-network
  (:require [lambda-ml.core :as c]
            [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(declare mean-squared-error)

(def bias (m/matrix [1.0]))
(def epsilon 0.0001)

(defn drop-bias
  [m]
  (m/submatrix m 1 [1 (dec (m/column-count m))]))

(defn feed-forward
  "Returns the activation values for nodes in a neural network after forward
  propagating the values of a single input example x through the network."
  [x theta]
  (reduce (fn [activations weights]
            (let [inputs (if (empty? activations) (m/matrix x) (last activations))
                  inputs+bias (m/join bias inputs)
                  outputs (m/emap c/sigmoid (m/mmul weights inputs+bias))]
              (conj activations outputs)))
          []
          theta))

(defn feed-forward-batch
  "Returns the activation values for nodes in a neural network after forward
  propagating a collection of input examples x through the network."
  [x theta]
  (-> (reduce (fn [inputs weights]
                (let [bias (m/broadcast 1.0 [1 (m/column-count inputs)])
                      inputs+bias (m/join bias inputs)
                      outputs (m/emap c/sigmoid (m/mmul weights inputs+bias))]
                  outputs))
              (m/transpose (m/matrix x))
              theta)
      (m/transpose)))

(defn back-propagate
  "Returns the errors of each node in a neural network after propagating the
  the errors at the output nodes, computed against a single target value y,
  backwards through the network."
  [y theta activations]
  (let [a (last activations)
        output-errors (m/matrix (m/mul (m/sub a y) a (m/sub 1 a)))]
    (->> (map vector (reverse (rest theta)) (reverse (butlast activations)))
         (reduce (fn [errors [w a]]
                   (cons (m/mul a (m/sub 1 a) (m/mmul (first errors) (drop-bias w)))
                         errors))
                 (list output-errors))
         (vec))))

(defn compute-gradients
  "Returns the gradients for each weight given activation values and errors on
  a input values of a single example x."
  [x activations errors]
  (->> (map vector errors (cons (m/matrix x) (butlast activations)))
       (reduce (fn [gradients [e a]]
                 (let [a (m/join bias a)]
                   (conj gradients (m/outer-product e a))))
               [])))

(defn numeric-gradients
  "Returns the numeric approximations of the gradients for each weight given the
  input values of a single example x and label y. Used for debugging by checking
  against the computed gradients during backpropagation."
  [x y theta]
  (mapv (fn [k weights]
          (m/matrix (for [i (range (m/row-count weights))]
                      (for [j (range (m/column-count weights))]
                        (let [w (m/select weights i j)
                              theta+ (assoc theta k (m/set-selection weights i j (+ w epsilon)))
                              theta- (assoc theta k (m/set-selection weights i j (- w epsilon)))]
                          (/ (- (mean-squared-error (list x) (list y) theta+)
                                (mean-squared-error (list x) (list y) theta-))
                             (* 2 epsilon)))))))
        (range)
        theta))

(defn gradient-descent-step
  "Performs a single gradient step on the input and target values of a single
  example x and label y, and returns the updated weights."
  [x y theta alpha lambda]
  (let [activations (feed-forward x theta)
        errors (back-propagate y theta activations)
        gradients (compute-gradients x activations errors)
        regularization (map (fn [w]
                              (-> (m/mul alpha lambda w)
                                  (m/set-column 0 (m/matrix (repeat (m/row-count w) 0)))))
                            theta)]
    ;; Numeric gradient checking
    ;(println (map (comp #(/ (m/esum %) (m/ecount %)) m/abs m/sub) gradients (numeric-gradients x y theta)))
    (mapv m/sub theta (m/mul gradients alpha) regularization)))

(defn gradient-descent
  "Performs gradient descent on input and target values of all examples x and
  y, and returns the updated weights."
  [model x y]
  (let [{alpha :alpha lambda :lambda theta :parameters step :step} model]
    (loop [inputs x
           targets y
           weights theta]
      (if (and (empty? inputs) (empty? targets))
        weights
        (recur (rest inputs)
               (rest targets)
               (step (first inputs) (first targets) weights alpha lambda))))))

(defn init-parameters
  [layers]
  (let [r (java.util.Random.)
        rand (fn [] (.nextGaussian r))]
    (->> (for [i (range (dec (count layers)))]
           (let [ni (inc (nth layers i))    ;; number of nodes at layer i (+ bias node)
                 ni+1 (nth layers (inc i))] ;; number of nodes at layer i+1
             ;; initialize random values as parameters
             (vec (repeatedly ni+1 #(vec (repeatedly ni rand))))))
         (mapv m/matrix))))

(defn mean-squared-error
  [x y theta]
  (/ (m/esum (m/square (m/sub (feed-forward-batch x theta) y)))
     2))

(defn predict
  [x theta]
  (mapv vec (feed-forward-batch x theta)))

(defn neural-network-fit
  "Trains a neural network model for the given training data. For new models,
  parameters are initialized as random values from a normal distribution."
  ([model data]
   (neural-network-fit model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{hidden :hidden layers :layers theta :parameters init :init} model
         layers (or layers
                    (concat [(count (first x))]   ;; number of input nodes
                            hidden                ;; number of nodes at each hidden layer
                            [(count (first y))])) ;; number of output nodes
         model (-> model
                   (assoc :layers layers)
                   (assoc :parameters (or theta (init layers))))]
     (assoc model :parameters (gradient-descent model x y)))))

(defn neural-network-predict
  "Predicts the values of example data using a neural network model."
  [model x]
  (let [{theta :parameters predict :predict} model]
    (when (not (nil? theta))
      (predict x theta))))

(defn neural-network-cost
  ([model data]
   (neural-network-cost model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{theta :parameters cost :cost} model]
     (when (not (nil? theta))
       (cost x y theta)))))

(defn print-neural-network
  "Prints information about a given neural network."
  [model]
  (println
   (cond-> model
     (contains? model :parameters)
     (assoc :parameters (clojure.string/join " -> "
                                             (for [thetai (:parameters model)]
                                               (str (dec (count (first thetai))) " x " (count thetai))))))))

(defn make-neural-network
  "Returns a neural network model where alpha is the learning rate and hidden is
  a sequence of numbers where the ith element is the number of nodes in the ith
  hidden layer."
  [hidden alpha lambda]
  {:alpha alpha
   :lambda lambda
   :hidden hidden
   :init init-parameters
   :step gradient-descent-step
   :cost mean-squared-error
   :predict predict})
