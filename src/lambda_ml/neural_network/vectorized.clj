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
          []
          theta))

(defn back-propagate
  [y theta activations]
  (let [a (last activations)
        errors (m/mul (m/sub y a) a (m/sub 1 a))]
   (->> (map vector (reverse (rest theta)) (reverse (butlast activations)))
        (reduce (fn [e [weights a]]
                  (cons (->> (weights-without-bias weights)
                             (m/mmul (last e))
                             (m/mul a (m/sub 1 a)))
                        e))
                (list errors))
        (vec))))
