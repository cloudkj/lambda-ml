(ns lambda-ml.ensemble
  "Ensemble learning methods.

  Example usage:
  ```
  (def data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]])
  (def tree
    (let [min-split 2
          min-leaf 1
          max-features 2]
      (make-classification-tree gini-impurity min-split min-leaf max-features)))
  (def fit
    (let [rate 1.0]
      (-> (iterate #(add-bagging-estimator % tree decision-tree-fit decision-tree-predict)
                    (make-bagging-classifier rate))
          (nth 1001)
          (bagging-ensemble-fit data))))
  (bagging-ensemble-predict fit (map butlast data))
  ;;=> (0 1 1 0)
  ```"
  (:require [lambda-ml.core :refer :all]))

(defn bagging-ensemble-fit
  "Fits an ensemble of estimators using bootstrap samples of the training data
  for each base estimators."
  ([ensemble data]
   (let [n (* (:rate ensemble) (count data))]
     (->> (:estimators ensemble)
          (map (fn [[m f p]] (f m (sample-with-replacement data n))))
          (assoc ensemble :fits))))
  ([ensemble x y]
   (bagging-ensemble-fit ensemble (map concat x (map list y)))))

(defn bagging-ensemble-predict
  "Predicts the values of example data using a bagging ensemble."
  [ensemble x]
  (->> (:fits ensemble)
       (map #(%1 %2 x) (map last (:estimators ensemble)))
       (apply map vector)
       (map (:aggregation ensemble))))

(defn add-bagging-estimator
  "Adds a base estimator to an ensemble, where each estimator is defined by fit
  and predict functions used for training on then predicting from the provided
  model, respectively."
  [ensemble model fit predict]
  (->> [model fit predict]
       (conj (get ensemble :estimators []))
       (assoc ensemble :estimators)))

(defn make-bagging-classifier
  "Returns a classifier based on an ensemble of classifiers to be fit to random
  samples of training data, where rate is the percent of data used to create
  each bootstrap sample. Predictions are aggregated across classifiers by taking
  the mode of predicted values."
  [rate]
  {:rate rate
   :aggregation mode})

(defn make-bagging-regressor
  "Returns a regressor based on an ensemble of regressors to be fit to random
  samples of training data, where rate is the percent of data used to create
  each bootstrap sample. Predictions are aggregated across regressors by taking
  the mean of predicted values."
  [rate]
  {:rate rate
   :aggregation mean})
