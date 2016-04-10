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

(defn encode-train
  [example]
  (let [x (mapv #(/ % 255.0) (rest example))
        y (-> (vec (replicate 10 0.0))
              (assoc (first example) 1.0))]
    [x y]))

(defn encode-test
  [example]
  (mapv #(/ % 255.0) example))

(def train-data
  (with-open [in (clojure.java.io/reader "train.csv")]
    (doall
     (->> (rest (csv/read-csv in))
          (map #(map read-string %))
          (map encode-train)))))

(def test-data
  (with-open [in (clojure.java.io/reader "test.csv")]
    (doall
     (->> (rest (csv/read-csv in))
          (map #(map read-string %))
          (map encode-test)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.digit-recognizer/test-data</span>","value":"#'lambda-ml.examples.kaggle.digit-recognizer/test-data"}
;; <=

;; @@
(defn square [x] (* x x))

(defn output->label
  [output]
  (first (apply max-key second (map-indexed vector output))))

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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.digit-recognizer/error</span>","value":"#'lambda-ml.examples.kaggle.digit-recognizer/error"}
;; <=

;; @@
(require :reload 'lambda-ml.neural-network)

;; Initial weights

(def theta
  (let [r (java.util.Random.)
        rand (fn [] (.nextGaussian r))]
    [(repeatedly 30 #(repeatedly (inc 784) rand))
     (repeatedly 10 #(repeatedly (inc 30) rand))]))

;; Parametrs and data

(def alpha 0.4)
(def train-set train-data)
(def error-set (random-sample train-data 100))
(def test-set (take-last 2000 train-data))
(def step 100)

(binding [*out* *err*] (println "initial accuracy:" (float (accuracy theta test-set))))

(def nn
  (loop [examples train-set
         errors []
         weights theta]
    (if (empty? examples)
      [weights errors]
      (let [[x y] (first examples)]
        (recur (rest examples)
               (if (= (mod (count examples) step) 0)
                 (let [i (- (count train-set) (count examples))
                       e (error weights error-set)]
                   (binding [*out* *err*] (println i e))
                   (conj errors [i e]))
                 errors)
               (gradient-descent-step x y weights alpha))))))

(binding [*out* *err*] (println "final accuracy:" (float (accuracy (first nn) test-set))))
;(plot/list-plot (second nn) :joined true :plot-range [:all [0 100]])

(println "ImageId,Label")
(doseq [[id x] (map-indexed vector test-data)]
  (let [output (last (feed-forward x (first nn)))
        label (output->label output)]
    (println (str (inc id) "," label))))
;; @@
;; ->
;;; initial accuracy: 0.1375
;;; 0 423.5910841962969
;;; 100 163.11651697951902
;;; 200 116.6679767752302
;;; 300 106.86560335092673
;;; 400 104.5675599668178
;;; 500 103.52783432947602
;;; 600 101.78596875950088
;;; 700 100.06152759644586
;;; 800 98.59406334757537
;;; 900 98.45093135119977
;;; final accuracy: 0.225
;;; ImageId,Label
;;; 1,2
;;; 2,4
;;; 3,4
;;; 4,4
;;; 5,8
;;; 6,5
;;; 7,5
;;; 8,8
;;; 9,4
;;; 10,8
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
