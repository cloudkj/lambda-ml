(ns lambda-ml.neural-network
  (:require [lambda-ml.core :as c]))

(defn feed-forward
  "Returns the activation values for nodes in a neural network after propagating
  the input values given as x forward through the network."
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
  [activation target]
  (* (- target activation) activation (- 1 activation)))

(defn hidden-node-error
  [activation weights deltas]
  (* activation (- 1 activation) (c/dot-product weights deltas)))

(defn back-propagate
  [y theta activations]
  (loop [i (dec (count theta))
         deltas []]
    (if (< i 0)
      deltas
      (let [ai (nth activations i)
            d (if (= i (dec (count theta)))
                (map output-node-error ai y)
                (map-indexed (fn [j a]
                               (let [weights (map #(nth % (inc j)) (nth theta (inc i)))]
                                 (hidden-node-error a weights (first deltas))))
                             ai))]
        (recur (dec i) (cons d deltas))))))

(defn gradient-step
  [x theta alpha activations deltas]
  (loop [i 0
         gradients []]
    (if (= i (count theta))
      gradients
      (let [activations (if (= i 0) x (nth activations (dec i)))
            deltas (nth deltas i)]
        (recur (inc i)
               (conj gradients
                     (map-indexed (fn [j weights]
                                    (map-indexed (fn [k w]
                                                   (let [error (nth deltas j)
                                                         output (if (= k 0) 1.0 (nth activations (dec k)))]
                                                     (* alpha error output)))
                                                 weights))
                                  (nth theta i))))))))
