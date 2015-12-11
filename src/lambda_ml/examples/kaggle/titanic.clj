(ns lambda-ml.examples.kaggle.titanic
  (require [clojure.data.csv :as csv]
           [lambda-ml.regression :refer :all]))

(defn convert
  [s]
  (cond (number? s) s
        (clojure.string/blank? s) nil
        :else (read-string s)))

(defn read-train
  [file]
  (with-open [in (clojure.java.io/reader file)]
    (doall
     (->> (rest (csv/read-csv in))
          (map (fn [[survival pclass name sex age sibsp parch ticket fare cabin embarked]]
                 (let [sex (if (= sex "male") 0 1)]
                   (map convert [pclass sex sibsp parch survival]))))))))

(defn read-test
  [file]
  (with-open [in (clojure.java.io/reader file)]
    (doall
     (->> (rest (csv/read-csv in))
          (map (fn [[id pclass name sex age sibsp parch ticket fare cabin embarked]]
                 (let [sex (if (= sex "male") 0 1)]
                   (cons id (map convert [pclass sex sibsp parch])))))))))

(defn -main
  []
  (let [train (read-train "train.csv")
        test (read-test "test.csv")
        alpha 0.01
        iters 1000
        threshold 0.5
        predictions (-> (make-logistic-regression alpha iters)
                        (regression-fit train)
                        (regression-predict (map rest test)))]
    (println "PassengerId,Survived")
    (doseq [[id survival] (map (fn [t p] [(first t) p]) test predictions)]
      (let [survival (if (> survival threshold) 1 0)]
        (println (str id "," survival))))))
