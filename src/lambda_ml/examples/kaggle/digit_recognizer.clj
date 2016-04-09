;; gorilla-repl.fileformat = 1

;; **
;;; # Digit Recognizer
;; **

;; @@
(ns lambda-ml.examples.kaggle.digit-recognizer
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.math.numeric-tower :refer :all]
            [lambda-ml.core :refer :all]
            [lambda-ml.neural-network :refer :all]))

(defn encode
  [example]
  (let [x (mapv #(/ % 255.0) (rest example))
        y (-> (vec (replicate 10 0.0))
              (assoc (first example) 1.0))]
    [x y]))

(defn read-file
  [file]
  (with-open [in (clojure.java.io/reader file)]
    (doall
     (->> (rest (csv/read-csv in))
          (map #(map read-string %))
          (map encode)))))
          

(def train (read-file "train.csv"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.digit-recognizer/train</span>","value":"#'lambda-ml.examples.kaggle.digit-recognizer/train"}
;; <=

;; @@
(require :reload 'lambda-ml.neural-network)

(defn square [x] (* x x))

(defn output->label
  [output]
  (first (apply max-key second (map-indexed vector output))))

(def theta
  (let [r (java.util.Random.)
        rand (fn [] (.nextGaussian r))]
    [(repeatedly 30 #(repeatedly (inc 784) rand))
     (repeatedly 10 #(repeatedly (inc 30) rand))]))

(def alpha 0.03)

(defn accuracy
  [weights examples]
  (/ (reduce + (for [example examples]
                 (let [[x y] example
                       output (last (feed-forward x weights))
                       prediction (output->label output)
                       label (output->label y)]
                   (if (= prediction label) 1 0))))
     (count examples)))

(defn error
  [weights examples]
  (reduce + (for [example examples]
              (let [[x y] example
                    output (last (feed-forward x weights))]
                (->> (map (comp square -) output y)
                     (reduce +))))))

(def train-set (take 1000 train))
(def error-set (random-sample train 100))
(def test-set (take-last 1000 train))

(println "initial accuracy:" (float (accuracy theta test-set)))

(time
  (def nn
    (loop [examples train-set
           weights theta]
      (when (= (mod (count examples) 10) 0)
        (do
          (println (count examples) (error weights error-set))))
      (if (empty? examples)
        weights
        (let [[x y] (first examples)]
          (recur (rest examples) 
                 (gradient-descent-step x y weights alpha)))))))

(println "final accuracy:" (float (accuracy nn test-set)))
;; @@
;; ->
;;; 20 570.5631298288349
;;; 10 563.4238857939789
;;; 0 554.110371442577
;;; &quot;Elapsed time: 2129.342157 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.digit-recognizer/nn</span>","value":"#'lambda-ml.examples.kaggle.digit-recognizer/nn"}
;; <=

;; @@

;; @@
