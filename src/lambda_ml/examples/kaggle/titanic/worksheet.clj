;; gorilla-repl.fileformat = 1

;; **
;;; # Lambda ML Example: Kaggle Titanic with Logistic Regression
;;; 
;;; An example of applying logistic regression to the data from the [Titanic: Machine Learning from Disaster](https://www.kaggle.com/c/titanic) competition from Kaggle.
;;; 
;;; First, lets set up our namespace and define a helper function for sanitizing data.
;; **

;; @@
(ns lambda-ml.examples.kaggle.titanic.worksheet
  (require [lambda-ml.regression :refer :all]
           [clojure.data.csv :as csv]
           [gorilla-plot.core :as plot]))

(defn sanitize
  [s]
  (cond (number? s) s
        (clojure.string/blank? s) nil
        :else (read-string s)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.titanic.worksheet/sanitize</span>","value":"#'lambda-ml.examples.kaggle.titanic.worksheet/sanitize"}
;; <=

;; **
;;; Load the training data. Note that we're being arbitrarily selective about columns since we'll be using only a subset of the features.
;; **

;; @@
(def train
  (with-open [in (clojure.java.io/reader "resources/examples/kaggle/titanic/train.csv")]
    (doall
     (->> (rest (csv/read-csv in))
          (map (fn [[id survival pclass name sex age sibsp parch ticket fare cabin embarked]]
                 (let [sex (if (= sex "male") 0 1)]
                   (map sanitize [pclass sex sibsp parch survival]))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.titanic.worksheet/train</span>","value":"#'lambda-ml.examples.kaggle.titanic.worksheet/train"}
;; <=

;; **
;;; Visualize the categorical features in the training data.
;; **

;; @@
(for [i [0 1 4]]
  (let [data (frequencies (map #(nth % i) train))]
    (plot/bar-chart (keys data) (vals data))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"4e27e1c3-c91a-4219-a6d5-fc304c0d1624","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"4e27e1c3-c91a-4219-a6d5-fc304c0d1624","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"4e27e1c3-c91a-4219-a6d5-fc304c0d1624"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"4e27e1c3-c91a-4219-a6d5-fc304c0d1624","values":[{"x":3,"y":491},{"x":1,"y":216},{"x":2,"y":184}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :values ({:x 3, :y 491} {:x 1, :y 216} {:x 2, :y 184})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0b902cc4-581e-4d58-bfef-8b257622e905","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0b902cc4-581e-4d58-bfef-8b257622e905","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"0b902cc4-581e-4d58-bfef-8b257622e905"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"0b902cc4-581e-4d58-bfef-8b257622e905","values":[{"x":0,"y":577},{"x":1,"y":314}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0b902cc4-581e-4d58-bfef-8b257622e905\", :values ({:x 0, :y 577} {:x 1, :y 314})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c41d24b7-ca6f-4b3d-98b7-2a473f66c330","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c41d24b7-ca6f-4b3d-98b7-2a473f66c330","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"c41d24b7-ca6f-4b3d-98b7-2a473f66c330"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"c41d24b7-ca6f-4b3d-98b7-2a473f66c330","values":[{"x":0,"y":549},{"x":1,"y":342}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :values ({:x 0, :y 549} {:x 1, :y 342})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}],"value":"(#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"4e27e1c3-c91a-4219-a6d5-fc304c0d1624\", :values ({:x 3, :y 491} {:x 1, :y 216} {:x 2, :y 184})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"0b902cc4-581e-4d58-bfef-8b257622e905\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"0b902cc4-581e-4d58-bfef-8b257622e905\", :values ({:x 0, :y 577} {:x 1, :y 314})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"c41d24b7-ca6f-4b3d-98b7-2a473f66c330\", :values ({:x 0, :y 549} {:x 1, :y 342})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}})"}
;; <=

;; **
;;; Visualize the numerical features in the training data.
;; **

;; @@
(for [i [2 3]]
  (plot/histogram (map #(nth % i) train)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"d97379af-c84a-4aa5-824d-f9bb36af4120","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"d97379af-c84a-4aa5-824d-f9bb36af4120","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"d97379af-c84a-4aa5-824d-f9bb36af4120"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"d97379af-c84a-4aa5-824d-f9bb36af4120","values":[{"x":0.0,"y":0},{"x":0.7272727272727274,"y":608.0},{"x":1.4545454545454548,"y":209.0},{"x":2.181818181818182,"y":28.0},{"x":2.9090909090909096,"y":0.0},{"x":3.636363636363637,"y":16.0},{"x":4.363636363636364,"y":18.0},{"x":5.090909090909092,"y":5.0},{"x":5.818181818181819,"y":0.0},{"x":6.545454545454547,"y":0.0},{"x":7.272727272727274,"y":0.0},{"x":8.000000000000002,"y":7.0},{"x":8.727272727272728,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :values ({:x 0.0, :y 0} {:x 0.7272727272727274, :y 608.0} {:x 1.4545454545454548, :y 209.0} {:x 2.181818181818182, :y 28.0} {:x 2.9090909090909096, :y 0.0} {:x 3.636363636363637, :y 16.0} {:x 4.363636363636364, :y 18.0} {:x 5.090909090909092, :y 5.0} {:x 5.818181818181819, :y 0.0} {:x 6.545454545454547, :y 0.0} {:x 7.272727272727274, :y 0.0} {:x 8.000000000000002, :y 7.0} {:x 8.727272727272728, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"cc29ab02-0a07-4279-9f94-dbdc2ce10a85","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"cc29ab02-0a07-4279-9f94-dbdc2ce10a85","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"cc29ab02-0a07-4279-9f94-dbdc2ce10a85"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"cc29ab02-0a07-4279-9f94-dbdc2ce10a85","values":[{"x":0.0,"y":0},{"x":0.5454545454545455,"y":678.0},{"x":1.090909090909091,"y":118.0},{"x":1.6363636363636367,"y":0.0},{"x":2.181818181818182,"y":80.0},{"x":2.7272727272727275,"y":0.0},{"x":3.272727272727273,"y":5.0},{"x":3.8181818181818183,"y":0.0},{"x":4.363636363636364,"y":4.0},{"x":4.90909090909091,"y":0.0},{"x":5.454545454545456,"y":5.0},{"x":6.000000000000002,"y":1.0},{"x":6.545454545454548,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :values ({:x 0.0, :y 0} {:x 0.5454545454545455, :y 678.0} {:x 1.090909090909091, :y 118.0} {:x 1.6363636363636367, :y 0.0} {:x 2.181818181818182, :y 80.0} {:x 2.7272727272727275, :y 0.0} {:x 3.272727272727273, :y 5.0} {:x 3.8181818181818183, :y 0.0} {:x 4.363636363636364, :y 4.0} {:x 4.90909090909091, :y 0.0} {:x 5.454545454545456, :y 5.0} {:x 6.000000000000002, :y 1.0} {:x 6.545454545454548, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}],"value":"(#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"d97379af-c84a-4aa5-824d-f9bb36af4120\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"d97379af-c84a-4aa5-824d-f9bb36af4120\", :values ({:x 0.0, :y 0} {:x 0.7272727272727274, :y 608.0} {:x 1.4545454545454548, :y 209.0} {:x 2.181818181818182, :y 28.0} {:x 2.9090909090909096, :y 0.0} {:x 3.636363636363637, :y 16.0} {:x 4.363636363636364, :y 18.0} {:x 5.090909090909092, :y 5.0} {:x 5.818181818181819, :y 0.0} {:x 6.545454545454547, :y 0.0} {:x 7.272727272727274, :y 0.0} {:x 8.000000000000002, :y 7.0} {:x 8.727272727272728, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"cc29ab02-0a07-4279-9f94-dbdc2ce10a85\", :values ({:x 0.0, :y 0} {:x 0.5454545454545455, :y 678.0} {:x 1.090909090909091, :y 118.0} {:x 1.6363636363636367, :y 0.0} {:x 2.181818181818182, :y 80.0} {:x 2.7272727272727275, :y 0.0} {:x 3.272727272727273, :y 5.0} {:x 3.8181818181818183, :y 0.0} {:x 4.363636363636364, :y 4.0} {:x 4.90909090909091, :y 0.0} {:x 5.454545454545456, :y 5.0} {:x 6.000000000000002, :y 1.0} {:x 6.545454545454548, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}})"}
;; <=

;; **
;;; Load the test data, which is structured slightly differently since we'll be making predictions on this data
;; **

;; @@
(def test
  (with-open [in (clojure.java.io/reader "resources/examples/kaggle/titanic/test.csv")]
    (doall
     (->> (rest (csv/read-csv in))
          (map (fn [[id pclass name sex age sibsp parch ticket fare cabin embarked]]
                 (let [sex (if (= sex "male") 0 1)]
                   (cons id (map sanitize [pclass sex sibsp parch])))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.titanic.worksheet/test</span>","value":"#'lambda-ml.examples.kaggle.titanic.worksheet/test"}
;; <=

;; **
;;; Define parameters and train a logistic regression model.
;; **

;; @@
(def alpha 0.01)
(def iters 1000)
(def threshold 0.5)

(def model (regression-fit (make-logistic-regression alpha iters) train))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.titanic.worksheet/model</span>","value":"#'lambda-ml.examples.kaggle.titanic.worksheet/model"}
;; <=

;; **
;;; Generate predictions using the model.
;; **

;; @@
(def predictions (regression-predict model (map rest test)))

(doseq [[id survival] (take 20 (map (fn [t p] [(first t) p]) test predictions))]
  (let [survival (if (> survival threshold) 1 0)]
    (println (str id "," survival))))
;; @@
;; ->
;;; 892,0
;;; 893,0
;;; 894,0
;;; 895,0
;;; 896,0
;;; 897,0
;;; 898,0
;;; 899,0
;;; 900,0
;;; 901,0
;;; 902,0
;;; 903,0
;;; 904,1
;;; 905,0
;;; 906,1
;;; 907,1
;;; 908,0
;;; 909,0
;;; 910,0
;;; 911,0
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
