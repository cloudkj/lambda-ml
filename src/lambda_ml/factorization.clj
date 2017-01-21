(ns lambda-ml.factorization
  "Unsupervised learning with non-negative matrix factorization.

  Example usage:
  ```
  (def data [[1 2 3] [4 5 6]])
  (let [dims 2]
    (-> (factorizations data dims)
        (nth 300)
        ((fn [x] (map #(mapv vec %) x)))))
  ;;=> ([[0.20900693256125408 0.2000948450048419]
  ;;=>   [0.8547267961216941  0.32426625588317753]]
  ;;=>  [[4.601094573778913   3.4274218917618486 2.1966425686791777]
  ;;=>   [0.20523936453382804 6.391048036139935  12.709895897835892]])
  ```"
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(defn init-factors
  [rows cols]
  (m/matrix (repeatedly rows #(repeatedly cols rand))))

(defn cost
  [a b]
  (m/esum (m/pow (m/sub a b) 2)))

(defn factorizations
  "Returns a lazy seq of factorizations of the input matrix v. For an m-by-n
  input matrix, each factorization is a pair of latent matrices with dimensions
  m-by-dims and dims-by-n."
  ([v dims]
   (factorizations (m/matrix v)
                   (init-factors (m/row-count v) dims)
                   (init-factors dims (m/column-count v))))
  ([v w h]
   (lazy-seq (let [h (m/emul h (m/div (m/mmul (m/transpose w) v)
                                      (m/mmul (m/transpose w) w h)))
                   ;; Note that `h` is updated before `w`
                   w (m/emul w (m/div (m/mmul v (m/transpose h))
                                      (m/mmul w h (m/transpose h))))]
               (cons [w h] (factorizations v w h))))))
