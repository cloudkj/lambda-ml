;; gorilla-repl.fileformat = 1

;; **
;;; # Digit Recognizer
;; **

;; @@
(ns lambda-ml.examples.kaggle.digit-recognizer
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.math.numeric-tower :refer :all]
            [lambda-ml.neural-network :refer :all]))

(defn read-file
  [file]
  (with-open [in (clojure.java.io/reader file)]
    (doall
     (->> (rest (csv/read-csv in))
          (map (fn [row] (map read-string row)))))))

(defn encode
  [example]
  (let [x (map #(float (/ % 255)) (rest example))
        y (-> (vec (replicate 10 0))
              (assoc (first example) 1))]
    [x y]))

(def train (read-file "train.csv"))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.digit-recognizer/train</span>","value":"#'lambda-ml.examples.kaggle.digit-recognizer/train"}
;; <=

;; @@
(require :reload 'lambda-ml.neural-network)

(def theta
  (let [r (java.util.Random.)
        rand (fn [] (.nextGaussian r))]
    [(repeatedly 30 #(repeatedly (inc 784) rand))
     (repeatedly 10 #(repeatedly (inc 30) rand))]))

(def alpha 0.03)

(defn square [x] (* x x))

(defn accuracy
  [weights examples]
  (/ (reduce + (for [example examples]
                 (let [[x y] (encode example)
                       output (last (feed-forward x weights))
                       prediction (first (apply max-key second (map-indexed vector output)))
                       label (first example)]
                   (if (= prediction label) 1 0))))
     (count examples)))

(defn error
  [weights examples]
  (reduce + (for [example examples]
              (let [[x y] (encode example) ;; TODO: can encode all examples at once
                    prediction (last (feed-forward x weights))]
                (->> (map (comp square -) prediction y)
                     (reduce +))))))

(def train-set (take 100 train))
(def test-set (take-last 100 train))

(println "initial accuracy:" (float (accuracy theta (take-last 1000 train))))

(def nn
  (loop [examples train-set
         weights theta]
    (when (= (mod (count examples) 10) 0)
      (do
        (println (count examples) (error weights test-set))))
    (if (empty? examples)
      weights
      (let [[x y] (encode (first examples))]
        (recur (rest examples) 
               (gradient-descent-step x y weights alpha))))))

(println "final accuracy:" (float (accuracy nn (take-last 1000 train))))
;; @@
;; ->
;;; initial accuracy: 0.096
;;; 1000 444.30308209320367
;;; 990 378.0147737339567
;;; 980 302.4564061322724
;;; 970 239.605532490668
;;; 960 212.29678997483956
;;; 950 196.42070110608557
;;; 940 188.50757423204607
;;; 930 184.48966292540197
;;; 920 181.2078287147166
;;; 910 177.041402446025
;;; 900 173.9101646539188
;;; 890 172.40550045600762
;;; 880 171.1575237116193
;;; 870 168.0990221945625
;;; 860 163.56141624933548
;;; 850 159.79437625307105
;;; 840 154.59905326264965
;;; 830 147.27606988270733
;;; 820 136.70927511248058
;;; 810 131.09725366236438
;;; 800 124.89377627964981
;;; 790 117.7318238154532
;;; 780 115.14331298554819
;;; 770 111.75372935611122
;;; 760 108.85819558280738
;;; 750 105.76327495295308
;;; 740 103.4328400399754
;;; 730 102.7611386679361
;;; 720 100.18796913002589
;;; 710 99.41237913447789
;;; 700 98.50102880124088
;;; 690 97.98476923234199
;;; 680 98.18035106098564
;;; 670 98.31239339391996
;;; 660 98.15378598334112
;;; 650 98.03855655881931
;;; 640 96.85809666543703
;;; 630 96.67921146501519
;;; 620 96.14044467083176
;;; 610 96.15632013515358
;;; 600 95.77176728002898
;;; 590 94.99983362428038
;;; 580 94.75148546745723
;;; 570 94.83176104628137
;;; 560 94.50487518278067
;;; 550 94.28112781261562
;;; 540 94.81437781080791
;;; 530 94.49730048395163
;;; 520 94.44281535599279
;;; 510 94.15237457257717
;;; 500 93.53220361580334
;;; 490 93.45169037820351
;;; 480 93.21449587659914
;;; 470 93.24386885253723
;;; 460 93.108182609636
;;; 450 92.79260043334409
;;; 440 92.54886309154284
;;; 430 92.32801981303399
;;; 420 92.34983836661681
;;; 410 92.18655992710507
;;; 400 92.19264728532576
;;; 390 92.41121236480068
;;; 380 92.67256336427457
;;; 370 92.77470730670768
;;; 360 92.70085966378404
;;; 350 92.41780397442889
;;; 340 92.31999686329924
;;; 330 92.22727712765085
;;; 320 92.3663012659501
;;; 310 92.36373253635037
;;; 300 92.62595572996936
;;; 290 92.59397165859077
;;; 280 92.63337334145655
;;; 270 93.3499179968866
;;; 260 93.41561301857034
;;; 250 93.92840749110393
;;; 240 93.40335005370443
;;; 230 93.36632714839827
;;; 220 93.64332213081798
;;; 210 93.63033292957032
;;; 200 93.77448141273173
;;; 190 93.63725260901025
;;; 180 93.30218211097792
;;; 170 93.45653618653976
;;; 160 93.21053003898378
;;; 150 92.80063372738967
;;; 140 92.84235481252865
;;; 130 92.73438748943161
;;; 120 92.81282298611956
;;; 110 92.24809395024609
;;; 100 92.26301957856202
;;; 90 92.57355944870694
;;; 80 92.12092151472247
;;; 70 91.51896634761908
;;; 60 92.43411470587634
;;; 50 92.21416138524145
;;; 40 91.51457483808026
;;; 30 91.90729563776367
;;; 20 91.81817786413164
;;; 10 92.0757816502463
;;; 0 92.47894202044974
;;; final accuracy: 0.178
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
