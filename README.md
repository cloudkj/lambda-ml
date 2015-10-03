# lambda-ml

A small machine learning library aimed at providing simple, concise
implementations of machine learning techniques and utilities. It is written in
Lisp (using the implementation du jour, Clojure) to maximize expressiveness and
enjoyment.

## Example Usage

### DBSCAN

    (ns example
     (:require [lambda-ml.clustering.dbscan :refer :all]
               [lambda-ml.distance :refer :all]))
    
    (def epsilon 4.0)
    (def min-pts 2)
    
    (dbscan euclidean epsilon min-pts [[2 10] [2 5] [8 4] [5 8] [7 5] [6 4] [1 2] [4 9]])
    ;;=> {[4 9] 2, [5 8] 2, [7 5] 1, [6 4] 1, [8 4] 1}

### K-means

    (ns example
     (:require [lambda-ml.clustering.k-means :refer :all]))
    
    (def k 2)
    (def iters 100)
    
    (nth (k-means k [[1 1] [1.5 2] [3 4] [5 7] [3.5 5] [4.5 5] [3.5 4.5]]) 100)
    ;;=> {0 ([3.5 4.5] [4.5 5] [3.5 5] [5 7] [3 4]), 1 ([1.5 2] [1 1])}

## Contents

### Algorithms

* DBSCAN
* K-means
* K-nearest neighbors
* Linear regression
* Logistic regression
* Naive Bayes

### Data Structures

* K-d tree
* Priority queue

## License

Copyright © 2015
