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
    (conj x y)))

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
(defn log [& more] (binding [*out* *err*] (apply println more)))

(defn output->label
  [output]
  (first (apply max-key second (map-indexed vector output))))

(defn accuracy
  [model validation]
  (let [labels (map (comp output->label last) validation)
        predictions (->> (map (comp vec butlast) validation)
                         (neural-network-predict model)
                         (map output->label))]
    (float (/ (reduce + (map (fn [a b] (if (= a b) 1 0)) labels predictions))
              (count validation)))))

(def alpha 0.5)
(def partitions 420)
(def iterations 3)

(defn train-neural-network
  [model data k]
  (let [validation (random-sample data 100)]
    (loop [partitions (partition (/ (count data) k) data)
           model model]
      (when (not (nil? (:parameters model)))
        (log "partition" (- k (count partitions)) (neural-network-cost model validation)))
      (if (empty? partitions)
        model
        (recur (rest partitions) (neural-network-fit model (first partitions)))))))

(def validation (take-last 2000 train-data))

(def nn
  (loop [iter 0
         model (make-neural-network [30] alpha)]
    (if (>= iter iterations)
      model
      (do
        (log "iteration" iter "accuracy" (accuracy model validation))
        (recur (inc iter) (train-neural-network model train-data partitions))))))

(log "final accuracy" (accuracy nn validation))

(println "ImageId,Label")
(doseq [[id output] (map-indexed vector (neural-network-predict nn test-data))]
  (println (str (inc id) "," (output->label output))))
;; @@
;; ->
;;; iteration 0 accuracy 0.0
;;; partition 1 96.06555535236717
;;; partition 2 96.55620177132641
;;; partition 3 91.65690495564674
;;; partition 4 82.2989226407048
;;; partition 5 78.27473520888623
;;; partition 6 76.47333963939181
;;; partition 7 74.17945178117628
;;; partition 8 72.26439298285767
;;; partition 9 69.57305954046464
;;; partition 10 66.33840011587513
;;; final accuracy 0.534
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
