# lambda-ml

A small machine learning library aimed at providing simple, concise
implementations of machine learning techniques and utilities. It is written in
Lisp (using the implementation du jour, Clojure) to maximize expressiveness and
enjoyment.

## Installation

Add the following dependency to your project:

[![Clojars Project](https://img.shields.io/clojars/v/lambda-ml.svg)](https://clojars.org/lambda-ml)

## Documentation

* [API Docs](https://cloudkj.github.io/lambda-ml/)

### Supervised Learning Algorithms

* [Artificial neural network](https://cloudkj.github.io/lambda-ml/lambda-ml.neural-network.html)
* [Decision tree](https://cloudkj.github.io/lambda-ml/lambda-ml.decision-tree.html)
* [Ensemble methods](https://cloudkj.github.io/lambda-ml/lambda-ml.ensemble.html)
* [K-nearest neighbors](https://cloudkj.github.io/lambda-ml/lambda-ml.nearest-neighbors.html)
* [Linear regression](https://cloudkj.github.io/lambda-ml/lambda-ml.regression.html)
* [Logistic regression](https://cloudkj.github.io/lambda-ml/lambda-ml.regression.html)
* [Naive Bayes](https://cloudkj.github.io/lambda-ml/lambda-ml.naive-bayes.html)
* [Random forest](https://cloudkj.github.io/lambda-ml/lambda-ml.random-forest.html)

### Unsupervised Learning Algorithms

* [DBSCAN](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.dbscan.html)
* [K-means](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.k-means.html)

### Data Structures

* [Binary tree](https://cloudkj.github.io/lambda-ml/lambda-ml.data.binary-tree.html)
* [K-d tree](https://cloudkj.github.io/lambda-ml/lambda-ml.data.kd-tree.html)
* [Priority queue](https://cloudkj.github.io/lambda-ml/lambda-ml.data.priority-queue.html)

## Examples

### Linear regression

```clojure
(ns example (:require [lambda-ml.regression :refer :all]))

(def data [[-2 -1] [1 1] [3 2]])
(def model (make-linear-regression 0.01 0 5000))
(def fit (regression-fit model data))

(regression-predict fit (map #(take 1 %) data))
;;=> (-0.9473684210526243 0.8684210526315812 2.0789473684210513)
```

### Logistic regression

```clojure
(ns example (:require [lambda-ml.regression :refer :all]))

(def data [[0.50 0] [0.75 0] [1.00 0] [1.25 0] [1.50 0] [1.75 0] [1.75 1] [2.00 0] [2.25 1] [2.50 0] [2.75 1] [3.00 0] [3.25 1] [3.50 0] [4.00 1] [4.25 1] [4.50 1] [4.75 1] [5.00 1] [5.50 1]])
(def model (make-logistic-regression 0.1 0 10000))
(def fit (regression-fit model data))

(fit :parameters)
;;=> (-4.077712444728931 1.5046450944762417)
```

### Naive Bayes

```clojure
(ns example (:require [lambda-ml.naive-bayes :refer :all]))

(def x [[6.0 180 12] [5.92 190 11] [5.58 170 12] [5.92 165 10] [5.0 100 6] [5.5 150 8] [5.42 130 7] [5.75 150 9]])
(def y [:male :male :male :male :female :female :female :female])
(def model (naive-bayes-fit x y))

(naive-bayes-predict model [[6.0 130 8]])
;;=> (:female)
```

## License

Copyright © 2015-2016
