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
* [Hierarchical agglomerative clustering](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.hierarchical.html)
* [K-means](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.k-means.html)
* [Non-negative matrix factorization](https://cloudkj.github.io/lambda-ml/lambda-ml.factorization.html)

## Examples

* [Classifying handwritten digits with an artificial neural network](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/kaggle/digit_recognizer.clj)
* [DBSCAN example](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/worksheets/dbscan.clj)
* [Decision tree example](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/worksheets/decision_tree.clj)
* [Hierarchical agglomerative clustering example](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/worksheets/hierarchical.clj)
* [K-means example](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/worksheets/k_means.clj)
* [Predicting survival on the Titanic with logistic regression](http://viewer.gorilla-repl.org/view.html?source=github&user=cloudkj&repo=lambda-ml&path=src/lambda_ml/examples/kaggle/titanic.clj)

## License

Copyright © 2015-2018
