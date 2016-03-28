(defproject lambda-ml "0.1.0-SNAPSHOT"
  :description "A small machine learning library aimed at providing simple, concise implementations of machine learning techniques and utilities."
  :url "http://github.com/cloudkj/lambda-ml"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-exec "0.3.6"]
            [lein-gorilla "0.3.6"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [gorilla-plot "0.1.4"]])
