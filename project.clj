(defproject lambda-ml "0.1.1"
  :description "A small machine learning library aimed at providing simple, concise implementations of machine learning techniques and utilities."
  :url "http://github.com/cloudkj/lambda-ml"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-ancient "0.6.15"]
            [lein-codox "0.10.1"]
            [lein-exec "0.3.6"]
            [lein-gorilla "0.3.6"]]
  :codox {:metadata {:doc/format :markdown}
          :namespaces [#"^lambda-ml\.(?!examples)"]
          :source-uri "https://github.com/cloudkj/lambda-ml/blob/master/{filepath}#L{line}"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [gorilla-plot "0.1.4"]
                 [net.mikera/core.matrix "0.61.0"]
                 [net.mikera/vectorz-clj "0.47.0"]])
