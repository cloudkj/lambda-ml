;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns charming-garden
  (:require [gorilla-plot.core :as plot]
            [lambda-ml.core :refer :all]))

(defn square [x] (* x x))

(def vector-with-intercept (comp vec (partial cons 1.0)))

(def data
  [[0.05 0.10 [0.01 0.99]]])

(def x
  (map butlast data))

(def y
  (map last data))

x

(def theta
  [[[0.35 0.15 0.20]
    [0.35 0.25 0.30]]
   [[0.60 0.40 0.45]
    [0.60 0.50 0.55]]])

(defn feed-forward
  "Returns activation values at each layer"
  [x theta]
  (for [xi x]
    (loop [j 0
           a [(vector-with-intercept xi)]]
      (let [out (for [thetaji (nth theta j)]
                  (sigmoid (dot-product (last a) thetaji)))]
        (if (>= j (dec (count theta)))
          (conj a out)
          (recur (inc j)
                 (conj a (vector-with-intercept out))))))))

(defn error
  [x y theta]
  (map (fn [outi yi]
         (->> (map (comp square -) outi yi)
              (map #(/ % 2))
              (reduce +)))
       (map last (feed-forward x theta))
       y))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;charming-garden/error</span>","value":"#'charming-garden/error"}
;; <=

;; @@
theta

(def a (feed-forward x theta))

a

(map - (last (first a)) (first y))

(map (fn [x] (* x (- 1 x))) (last (first a)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.18681560180895948</span>","value":"0.18681560180895948"},{"type":"html","content":"<span class='clj-double'>0.17551005281727122</span>","value":"0.17551005281727122"}],"value":"(0.18681560180895948 0.17551005281727122)"}
;; <=

;; @@

;; @@
