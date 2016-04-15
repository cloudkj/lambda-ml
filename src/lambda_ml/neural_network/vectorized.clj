(ns lambda-ml.neural-network.vectorized
  (:require [lambda-ml.core :as c]
            [clojure.core.matrix :as m]))

(defn weights-without-bias
  [weights]
  (m/submatrix weights 1 [1 (dec (m/column-count weights))]))

(defn feed-forward
  "Returns a sequence of matrices representing the activation values for the
  given inputs. x is a matrix of input values, where each row is an input
  example, and theta is a sequence of matrices, where the ith matrix contains
  the weights between the ith and i+1th layers of the network."
  [x theta]
  (reduce (fn [activations weights]
            (let [inputs (if (empty? activations) x (last activations))
                  inputs+bias (->> (m/transpose inputs)
                                   (m/join (m/broadcast 1.0 [1 (m/row-count x)]))
                                   (m/transpose))
                  outputs (->> (m/mmul inputs+bias (m/transpose weights))
                               (m/emap c/sigmoid))]
              (conj activations outputs)))
          (vector)
          theta))

(defn back-propagate
  [y theta activations]
  (let [a (last activations)
        output-errors (m/mul (m/sub y a) a (m/sub 1 a))]
    (->> (map vector (reverse (rest theta)) (reverse (butlast activations)))
         (reduce (fn [errors [w a]]
                   (cons (->> (weights-without-bias w)
                              (m/mmul (first errors))
                              (m/mul a (m/sub 1 a)))
                         errors))
                 (list output-errors))
         (vec))))

(defn gradient-descent
  "Performs gradient descent on matrices of input and target values x and y, and
  returns a sequence of matrices representing the updated weights."
  [x y theta alpha]
  (let [x (m/matrix x)
        y (m/matrix y)
        theta (map m/matrix theta)
        activations (feed-forward x theta)
        errors (back-propagate y theta activations)]
    (->> (map vector errors (cons x (butlast activations)) theta)
         (reduce (fn [weights [e a t]]
                   (let [a (m/join-along 1 (m/broadcast 1.0 [(m/row-count a) 1]) a)]
                     (->> (m/mul alpha (m/mmul (m/transpose e) a)) ;; gradients
                          (m/add t)                                ;; weights + gradients
                          (conj weights))))
                 (vector)))))
