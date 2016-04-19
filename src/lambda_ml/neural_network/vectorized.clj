(ns lambda-ml.neural-network.vectorized
  (:require [lambda-ml.core :as c]
            [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(def bias (m/matrix [1.0]))

(defn drop-bias
  [m]
  (m/submatrix m 1 [1 (dec (m/column-count m))]))

(defn feed-forward
  ;; TODO: make sure input `theta` is a sequence of matrices! otherwise function will be slow...
  [x theta]
  (reduce (fn [activations weights]
            (let [inputs (if (empty? activations) (m/matrix x) (last activations))
                  inputs+bias (m/join bias inputs)
                  outputs (m/emap c/sigmoid (m/mmul weights inputs+bias))]
              (conj activations outputs)))
          []
          theta))

(defn back-propagate
  ;; TODO: make sure input `theta` is a sequence of matrices! otherwise function will be slow...
  [y theta activations]
  (let [a (last activations)
        output-errors (m/matrix (m/mul (m/sub y a) a (m/sub 1 a)))]
    (->> (map vector (reverse (rest theta)) (reverse (butlast activations)))
         (reduce (fn [errors [w a]]
                   (cons (m/mul a (m/sub 1 a) (m/mmul (first errors) (drop-bias w)))
                         errors))
                 (list output-errors))
         (vec))))

(defn compute-gradients
  [x alpha activations errors]
  (->> (map vector errors (cons (m/matrix x) (butlast activations)))
       (reduce (fn [gradients [e a]]
                 (let [a (m/mul alpha (m/join bias a))]
                   (conj gradients (m/outer-product e a))))
               [])))

(defn gradient-descent-step
  ;; TODO: make sure input 'theta' is a sequence of matrices
  [x y theta alpha]
  (let [activations (feed-forward x theta)
        errors (back-propagate y theta activations)
        gradients (compute-gradients x alpha activations errors)]
    (mapv m/add theta gradients)))

(defn feed-forward-batch
  [x theta]
  (-> (reduce (fn [inputs weights]
                (let [bias (m/broadcast 1.0 [1 (m/column-count inputs)])
                      inputs+bias (m/join bias inputs)
                      outputs (m/emap c/sigmoid (m/mmul weights inputs+bias))]
                  outputs))
              (m/transpose (m/matrix x))
              theta)
      (m/transpose)))
