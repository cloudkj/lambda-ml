(ns lambda-ml.neural-network
  (:require [lambda-ml.core :as c]))

(defn feed-forward
  "Returns the activation values for nodes in a neural network after forward
  propagating the values of a single input example x through the network."
  [x theta]
  (loop [i 0
         activations []]
    (if (= i (count theta))
      activations
      (let [weights (nth theta i)
            input (if (= i 0) x (last activations))
            input+bias (c/vector-with-intercept input)
            output (mapv #(c/sigmoid (c/dot-product % input+bias)) weights)]
        (recur (inc i) (conj activations output))))))

(defn output-node-error
  "Returns the error of an output node, where a is the activation value of the
  output node and t is the target value (label) of the node."
  [a t]
  (* (- t a) a (- 1 a)))

(defn hidden-node-error
  "Returns the error of a hidden node, where a is the activation value of the
  node, w is the weights feeding out of the node, and d is the errors of the
  nodes in the next layer. Formally, for the kth node in the ith layer:
  error_ik = a_ik * (1 - a_ik) * sum_{j in i+1} w_i+1,j * error_i+1,j"
  [a w d]
  (* a (- 1 a) (c/dot-product w d)))

(defn back-propagate
  "Returns the errors of each node in a neural network after propagating the
  the errors at the output nodes, computed against a single target value y,
  backwards through the network."
  [y theta activations]
  (loop [i (dec (count theta))
         errors (list)]
    (if (< i 0)
      (vec errors)
      (let [ai (nth activations i) ;; activations at layer i
            deltas (if (= i (dec (count theta)))
                     ;; output layer
                     (mapv output-node-error ai y)
                     ;; hidden layer(s)
                     (let [di+1 (first errors)] ;; errors at layer i + 1
                       (vec
                        (map-indexed (fn [j a]
                                       ;; weights feeding out of node j in layer i
                                       (let [weights (map #(nth % (inc j)) (nth theta (inc i)))]
                                         (hidden-node-error a weights di+1)))
                                     ai))))]
        (recur (dec i) (cons deltas errors))))))

(defn compute-gradients
  "Returns the gradients for each weight given activation values and errors on
  a input values of a single example x."
  [x theta alpha activations errors]
  (loop [i 0
         gradients []]
    (if (= i (count theta))
      gradients
      (let [ei (nth errors i)
            ;; x is used for the first layer since activations do not include input values
            activations (if (= i 0) x (nth activations (dec i)))
            ;; gradients for weights going from layer i to i + 1
            g (map-indexed (fn [j weights]
                             (let [error (nth ei j)]
                               (for [k (range (count weights))]
                                 (if (= k 0)
                                   (* alpha error) ;; output for bias node is constant
                                   (* alpha error (nth activations (dec k)))))))
                           (nth theta i))]
        (recur (inc i) (conj gradients g))))))

(defn gradient-descent-step
  "Performs a single gradient step on the input and target values of a single
  example x and label y, and returns the updated weights."
  [x y theta alpha]
  (let [activations (feed-forward x theta)
        errors (back-propagate y theta activations)
        gradients (compute-gradients x theta alpha activations errors)]
    ;; update weights with computed gradients
    ;; TODO: can do update directly after gradient computation
    (mapv (fn [ti gi]
            (mapv (fn [w g] (mapv + w g))
                  ti gi))
          theta
          gradients)))

(defn gradient-descent
  "Performs gradient descent on input and target values of all examples x and
  y, and returns the updated weights."
  [x y theta alpha]
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
                                    alpha)))))

(defn init-parameters
  [layers]
  (let [r (java.util.Random.)
        rand (fn [] (.nextGaussian r))]
    (vec
     (for [i (range (dec (count layers)))]
       (let [ni (inc (nth layers i))    ;; number of nodes at layer i (+ bias node)
             ni+1 (nth layers (inc i))] ;; number of nodes at layer i+1
         ;; initialize random values as parameters
         (vec (repeatedly ni+1 #(vec (repeatedly ni rand)))))))))

(defn neural-network-fit
  "Trains a neural network model for the given training data. For new models,
  parameters are initialized as random values from a normal distribution."
  ([model data]
   (neural-network-fit model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{alpha :alpha hidden :hidden layers :layers theta :parameters init :initialize} model
         layers (or layers
                    (concat [(count (first x))]   ;; number of input nodes
                            hidden                ;; number of nodes at each hidden layer
                            [(count (first y))])) ;; number of output nodes
         theta (or theta (init layers))]
     (-> model
         (assoc :layers layers)
         (assoc :parameters (gradient-descent x y theta alpha))))))

(defn neural-network-predict
  "Predicts the values of example data using a neural network model."
  [model x]
  (let [{theta :parameters} model]
    (when (not (nil? theta))
      (map (fn [xi] (last (feed-forward xi theta))) x))))

(defn neural-network-cost
  ([model data]
   (neural-network-cost model (map (comp vec butlast) data) (map (comp vec last) data)))
  ([model x y]
   (let [{theta :parameters} model]
     (reduce + (map (fn [xi yi]
                      (let [output (last (feed-forward xi theta))]
                        (reduce + (map (comp #(* % %) -) output yi))))
                    x y)))))

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
  [hidden alpha]
  {:alpha alpha
   :hidden hidden
   :initialize init-parameters})
