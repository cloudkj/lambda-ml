;; gorilla-repl.fileformat = 1

;; **
;;; # Foo
;; **

;; @@
(ns lambda-ml.examples.kaggle.march-madness
  (require [lambda-ml.core :refer :all]
           [lambda-ml.metrics :refer :all]
           [lambda-ml.regression :refer :all]
           [clojure.data.csv :as csv]
           [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def teams
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/Teams.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m [id name]] (assoc m (read-string id) name)) {})))))

(def slots
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/TourneySlots.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m [season slot hi lo]]
                     (let [keys [(read-string season) slot]
                           val [hi lo]]
                       (assoc-in m keys val)))
                   {})))))

(def seeds
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/TourneySeeds.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m [season seed team]]
                     (assoc-in m [(read-string season) seed] (read-string team)))
                   {})))))

(defn parse-seed
  [s]
  (Integer/parseInt (subs s 1 3)))

(def seeds-index
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/TourneySeeds.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m [season seed team]]
                     (assoc-in m (map read-string [season team]) (parse-seed seed)))
                   {})))))

(def tourney-results
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/TourneyCompactResults.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m row]
                     (let [[season daynum wteam wscore lteam lscore wloc numot] (map read-string row)]
                       (-> m
                           (assoc-in [season wteam lteam] {:score wscore :oppscore lscore})
                           (assoc-in [season lteam wteam] {:score lscore :oppscore wscore}))))
                   {})))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/tourney-results</span>","value":"#'lambda-ml.examples.kaggle.march-madness/tourney-results"}
;; <=

;; @@
(def stats
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/RegularSeasonCompactResults.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m row]
                     (let [[season daynum wteam wscore lteam lscore wloc numot] (map read-string row)]
                       (-> m
                           (update-in [season wteam] (fn [s]
                                                       (-> (or s {:g 0 :w 0 :l 0 :pf 0 :pa 0 :streak 0})
                                                           (update-in [:w] inc)
                                                           (update-in [:streak] (fn [x] (if (> x 0) (inc x) 1)))
                                                           (update-in [:pf] (partial + wscore))
                                                           (update-in [:pa] (partial + lscore)))))
                           (update-in [season lteam] (fn [s]
                                                       (-> (or s {:g 0 :w 0 :l 0 :pf 0 :pa 0 :streak 0})
                                                           (update-in [:l] inc)
                                                           (update-in [:streak] (fn [x] (if (< x 0) (dec x) -1)))
                                                           (update-in [:pf] (partial + lscore))
                                                           (update-in [:pa] (partial + wscore))))))))
                   {})))))

(defn win-percentage
  [year team]
  (let [w (get-in stats [year team :w])
        l (get-in stats [year team :l])]
    (float (/ w (+ w l)))))

(defn points-ratio
  [year team]
  (let [pf (get-in stats [year team :pf])
        pa (get-in stats [year team :pa])]
	(float (/ pf pa))))

(defn streak
  [year team]
  (get-in stats [year team :streak]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/streak</span>","value":"#'lambda-ml.examples.kaggle.march-madness/streak"}
;; <=

;; @@
(defn generate-index
  ([year]
   (generate-index year (seeds year) {}))
  ([year index prev]
   (if (<= (count index) (count prev))
     index
     (let [updated (loop [slots (slots year)
                          result index]
                     (if (empty? slots)
                       result
                       (let [[slot [hi lo]] (first slots)
                             hiteam (get-in index [hi])
                             loteam (get-in index [lo])
                             scores (get-in tourney-results [year hiteam loteam])]
                         (if (and hiteam loteam scores)
                           (let [winner (if (> (:score scores) (:oppscore scores)) hiteam loteam)]
                             (recur (rest slots) (assoc-in result [slot] winner)))
                           (recur (rest slots) result)))))]
       (generate-index year updated index)))))

(defn parse-round
  [slot]
  (if (= "R" (subs slot 0 1))
    (Integer/parseInt (subs slot 1 2))
    0))

(defn generate-matchups
  [year]
  (let [index (generate-index year)]
    (for [[slot t] (slots year)
          :let [[hi lo] (map index t)]
          :when (and hi lo)]
      (let [round (parse-round slot)
            hiseed (get-in seeds-index [year hi])
            loseed (get-in seeds-index [year lo])
            scores (get-in tourney-results [year hi lo])
            ; TODO: features go here
            matchup [year round hi lo
                     (teams hi)
                     (teams lo)
                     (win-percentage year hi)
                     (win-percentage year lo)
                     (points-ratio year hi)
                     (points-ratio year lo)
                     (streak year hi)
                     (streak year lo)
                     hiseed
                     loseed]]
        (if scores
          (let [winner (if (> (:score scores) (:oppscore scores)) 0 1)]
            (conj matchup winner))
          matchup)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/generate-matchups</span>","value":"#'lambda-ml.examples.kaggle.march-madness/generate-matchups"}
;; <=

;; @@
(def training-set
  (apply concat
         (for [year (range 1985 2012)]
           (generate-matchups year))))

(def test-set
  (apply concat
         (for [year (range 2012 2016)]
           (generate-matchups year))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/test-set</span>","value":"#'lambda-ml.examples.kaggle.march-madness/test-set"}
;; <=

;; **
;;; Baseline model that always picks the high seed.
;; **

;; @@
(def baseline-predictions
  (map (fn [matchup]
       (let [hiseed (last (drop-last 2 matchup))
             loseed (last (drop-last matchup))]
         (if (<= hiseed loseed) 0 1)))
     test-set))

(def baseline-accuracy
  (float (/ (count (filter identity (map = (map last test-set) baseline-predictions)))
            (count test-set))))

(def baseline-roc (roc-curve (map last test-set) baseline-predictions))

(println "baseline accuracy =" baseline-accuracy)
;; @@
;; ->
;;; baseline accuracy = 0.6865672
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn encode-features
  [matchup]
  (let [[year round hi lo
         hiteam loteam
         hiwinpct lowinpct
         hiptsratio loptsratio
         histreak lostreak
         hiseed loseed winner] matchup]
    [hiwinpct lowinpct
     hiptsratio loptsratio
     histreak lostreak
     ;hiseed loseed
     winner]))

(def alpha 0.01)
(def lambda 0.1)
(def iters 1000)
(def model (regression-fit (make-logistic-regression alpha lambda iters) (map encode-features training-set)))

(model :parameters)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.2863883644798515</span>","value":"0.2863883644798515"},{"type":"html","content":"<span class='clj-double'>0.1682624773818952</span>","value":"0.1682624773818952"},{"type":"html","content":"<span class='clj-double'>0.17721208508926406</span>","value":"0.17721208508926406"},{"type":"html","content":"<span class='clj-double'>-0.5217258622996755</span>","value":"-0.5217258622996755"},{"type":"html","content":"<span class='clj-double'>-0.46868653797720844</span>","value":"-0.46868653797720844"},{"type":"html","content":"<span class='clj-double'>-0.030497317361941514</span>","value":"-0.030497317361941514"},{"type":"html","content":"<span class='clj-double'>-0.05598093626419186</span>","value":"-0.05598093626419186"}],"value":"(0.2863883644798515 0.1682624773818952 0.17721208508926406 -0.5217258622996755 -0.46868653797720844 -0.030497317361941514 -0.05598093626419186)"}
;; <=

;; @@
(def predictions (regression-predict model (map (comp butlast encode-features) test-set)))
(def threshold 0.5)

(def accuracy
  (float (/ (count (filter identity (map = (map last test-set) (map #(if (<= % threshold) 0 1) predictions))))
            (count test-set))))

(println "accuracy =" accuracy)
;; @@
;; ->
;;; accuracy = 0.6865672
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
