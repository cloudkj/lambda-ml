(ns lambda-ml.neural-network
  "Multilayer perceptron neural network learning using backpropagation.

  Example usage:
  ```
  (def data [[0 0 [0]] [0 1 [1]] [1 0 [1]] [1 1 [0]]])
  (def fit
    (let [alpha 0.5
          lambda 0.001
          model (-> (make-neural-network alpha lambda)
                    (add-neural-network-layer 2 sigmoid)   ;; input layer
                    (add-neural-network-layer 3 sigmoid)   ;; hidden layer
                    (add-neural-network-layer 1 sigmoid))] ;; output layer
      (-> (iterate #(neural-network-fit % data) model)
          (nth 5000))))
  (neural-network-predict fit (map butlast data))
  ;;=> [[0.04262340225834812] [0.9582632706756758] [0.9581124103456861] [0.04103544440312673]]
  ```"
  (:require [lambda-ml.core :as c]
            [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(def bias (m/matrix [1.0]))
(def epsilon 0.0001)

(defn drop-bias
  [m]
  (m/submatrix m 1 [1 (dec (m/column-count m))]))

(defn feed-forward
  "Returns the activation values for nodes in a neural network after forward
  propagating the values of a single input example x through the network."
  [x theta fns]
  (reduce (fn [activations [weights f]]
            (let [inputs (if (empty? activations) (m/matrix x) (last activations))
                  inputs+bias (m/join bias inputs)
                  outputs (m/emap f (m/mmul weights inputs+bias))]
              (conj activations outputs)))
          []
          (map vector theta fns)))

(defn feed-forward-batch
  "Returns the activation values for nodes in a neural network after forward
  propagating a collection of input examples x through the network."
  [x theta fns]
  (-> (reduce (fn [inputs [weights f]]
                (let [bias (m/broadcast 1.0 [1 (m/column-count inputs)])
                      inputs+bias (m/join bias inputs)
                      outputs (m/emap f (m/mmul weights inputs+bias))]
                  outputs))
              (m/transpose (m/matrix x))
              (map vector theta fns))
      (m/transpose)))

(defn back-propagate
  "Returns the errors of each node in a neural network after propagating the
  the errors at the output nodes, computed against a single target value y,
  backwards through the network."
  [y theta fns' activations output-error]
  (->> (map vector
            (reverse (rest theta))
            (reverse (butlast activations))
            (reverse (butlast fns')))
       (reduce (fn [errors [w a f]]
                 (cons (m/mul (m/emap f a) (m/mmul (first errors) (drop-bias w)))
                       errors))
               (list (output-error y (last activations) (last fns'))))
       (vec)))

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
  [x y theta fns cost]
  (mapv (fn [k weights]
          (m/matrix (for [i (range (m/row-count weights))]
                      (for [j (range (m/column-count weights))]
                        (let [w (m/select weights i j)
                              theta+ (assoc theta k (m/set-selection weights i j (+ w epsilon)))
                              theta- (assoc theta k (m/set-selection weights i j (- w epsilon)))]
                          (/ (- (cost (list x) (list y) theta+ fns)
                                (cost (list x) (list y) theta- fns))
                             (* 2 epsilon)))))))
        (range)
        theta))

(defn gradient-descent-step
  "Performs a single gradient step on the input and target values of a single
  example x and label y, and returns the updated weights."
  [x y theta fns alpha lambda cost output-error]
  (let [activations (feed-forward x theta fns)
        errors (back-propagate y theta (map c/derivative fns) activations output-error)
        gradients (compute-gradients x activations errors)
        regularization (map (fn [w]
                              (-> (m/mul alpha lambda w)
                                  (m/set-column 0 (m/matrix (repeat (m/row-count w) 0)))))
                            theta)]
    ;; Numeric gradient checking
    ;;(println (map (comp #(/ (m/esum %) (m/ecount %)) m/abs m/sub) gradients (numeric-gradients x y theta fns cost)))
    (mapv m/sub theta (map #(m/mul % alpha) gradients) regularization)))

(defn gradient-descent
  "Performs gradient descent on input and target values of all examples x and
  y, and returns the updated weights."
  [model x y]
  (let [{alpha :alpha lambda :lambda theta :parameters cost :cost
         fns :activation-fns output-error :output-error} model]
    (loop [inputs x
           targets y
           weights theta]
      (if (and (empty? inputs) (empty? targets))
        weights
        (recur (rest inputs)
               (rest targets)
               (gradient-descent-step (first inputs)
                                      (first targets)
                                      weights
                                      fns
                                      alpha
                                      lambda
                                      cost
                                      output-error))))))

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

;; Cost functions

(defn cross-entropy-cost
  [x y theta fns]
  (let [a (feed-forward-batch x theta fns)]
    (/ (m/esum (m/add (m/mul y (m/log a))
                      (m/mul (m/sub 1 y) (m/log (m/sub 1 a)))))
       (- (count x)))))

(defn cross-entropy-output-error
  [y activations f']
  ;; Cross entropy error is independent of the derivative of output activation
  (m/sub activations y))

(defn quadratic-cost
  [x y theta fns]
  (/ (m/esum (m/square (m/sub (feed-forward-batch x theta fns) y)))
     2))

(defn quadratic-output-error
  [y activations f']
  (m/mul (m/sub activations y) (m/emap f' activations)))

;; API

(defn neural-network-fit
  "Trains a neural network model for the given training data. For new models,
  parameters are initialized as random values from a normal distribution."
  ([model data]
   (neural-network-fit model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{layers :layers theta :parameters} model
         model (-> model
                   (assoc :parameters (or theta (init-parameters layers))))]
     (assoc model :parameters (gradient-descent model x y)))))

(defn neural-network-predict
  "Predicts the values of example data using a neural network model."
  [model x]
  (let [{theta :parameters fns :activation-fns} model]
    (when (not (nil? theta))
      (mapv vec (feed-forward-batch x theta fns)))))

(defn neural-network-cost
  ([model data]
   (neural-network-cost model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{theta :parameters fns :activation-fns cost :cost} model]
     (when (not (nil? theta))
       (cost x y theta fns)))))

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
  "Returns a neural network model where alpha is the learning rate."
  ([alpha lambda]
   (make-neural-network alpha lambda cross-entropy-cost))
  ([alpha lambda cost]
   {:alpha alpha
    :lambda lambda
    :layers []
    :activation-fns []
    :cost cost
    :output-error (cond
                    (= cost cross-entropy-cost) cross-entropy-output-error
                    (= cost quadratic-cost)     quadratic-output-error)}))

(defn add-neural-network-layer
  "Adds a layer to a neural network model with n nodes and an activation
  function f."
  [model n f]
  (-> model
      (update :layers #(conj % n))
      (update :activation-fns #(conj % f))))
