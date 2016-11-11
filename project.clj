(defproject lambda-ml "0.1.0-SNAPSHOT"
  :description "A small machine learning library aimed at providing simple, concise implementations of machine learning techniques and utilities."
  :url "http://github.com/cloudkj/lambda-ml"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-codox "0.10.1"]
            [lein-exec "0.3.6"]
            [lein-gorilla "0.3.6"]]
  :codox {:namespaces [#"^lambda-ml\.(?!examples)"]
          :source-uri "https://github.com/cloudkj/lambda-ml/blob/master/{filepath}#L{line}"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [gorilla-plot "0.1.4"]
                 [net.mikera/core.matrix "0.51.0"]
                 [net.mikera/vectorz-clj "0.44.0"]])
