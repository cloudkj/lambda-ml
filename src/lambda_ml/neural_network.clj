(ns lambda-ml.neural-network
  (:require [lambda-ml.core :as c]))

(defn feed-forward
  "Returns the activation values for nodes in a neural network after forward
  propagating the input values x through the network."
  [x theta]
  (loop [i 0
         activations []]
    (if (= i (count theta))
      activations
      (let [weights (nth theta i)
            input (if (= i 0) x (last activations))
            input+bias (c/vector-with-intercept input)
            output (map #(c/sigmoid (c/dot-product % input+bias)) weights)]
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
  the errors at the output nodes, computed against target values y, backwards
  through the network."
  [y theta activations]
  (loop [i (dec (count theta))
         errors []]
    (if (< i 0)
      errors
      (let [ai (nth activations i) ;; activations at layer i
            di+1 (first errors)    ;; errors at layer i + 1
            deltas (if (= i (dec (count theta)))
                     (map output-node-error ai y)
                     (map-indexed (fn [j a]
                                    ;; weights feeding out of node j in layer i
                                    (let [weights (map #(nth % (inc j)) (nth theta (inc i)))]
                                      (hidden-node-error a weights di+1)))
                                  ai))]
        (recur (dec i) (cons deltas errors))))))

(defn compute-gradients
  "Returns the gradients for each weight given activation values and errors."
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
                             (map-indexed (fn [k w]
                                            (let [error (nth ei j)
                                                  output (if (= k 0)
                                                           1.0 ;; bias output
                                                           (nth activations (dec k)))]
                                              (* alpha error output)))
                                          weights))
                           (nth theta i))]
        (recur (inc i) (conj gradients g))))))

(defn gradient-descent-step
  [x y theta alpha]
  (let [activations (feed-forward x theta)
        errors (back-propagate y theta activations)
        gradients (compute-gradients x theta alpha activations errors)]
    (map (fn [ti gi]
           (map (fn [w g] (map + w g))
                ti gi))
         theta
         gradients)))
