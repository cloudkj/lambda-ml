;; gorilla-repl.fileformat = 1

;; **
;;; # Foo
;; **

;; @@
(ns lambda-ml.examples.kaggle.march-madness
  (require [lambda-ml.core :refer :all]
           [clojure.data.csv :as csv]))
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

(take 10 teams)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1348</span>","value":"1348"},{"type":"html","content":"<span class='clj-string'>&quot;Rhode Island&quot;</span>","value":"\"Rhode Island\""}],"value":"[1348 \"Rhode Island\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1244</span>","value":"1244"},{"type":"html","content":"<span class='clj-string'>&quot;Kennesaw&quot;</span>","value":"\"Kennesaw\""}],"value":"[1244 \"Kennesaw\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1368</span>","value":"1368"},{"type":"html","content":"<span class='clj-string'>&quot;SE Louisiana&quot;</span>","value":"\"SE Louisiana\""}],"value":"[1368 \"SE Louisiana\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1282</span>","value":"1282"},{"type":"html","content":"<span class='clj-string'>&quot;Missouri KC&quot;</span>","value":"\"Missouri KC\""}],"value":"[1282 \"Missouri KC\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1218</span>","value":"1218"},{"type":"html","content":"<span class='clj-string'>&quot;Hawaii&quot;</span>","value":"\"Hawaii\""}],"value":"[1218 \"Hawaii\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1425</span>","value":"1425"},{"type":"html","content":"<span class='clj-string'>&quot;USC&quot;</span>","value":"\"USC\""}],"value":"[1425 \"USC\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1371</span>","value":"1371"},{"type":"html","content":"<span class='clj-string'>&quot;Seton Hall&quot;</span>","value":"\"Seton Hall\""}],"value":"[1371 \"Seton Hall\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1305</span>","value":"1305"},{"type":"html","content":"<span class='clj-string'>&quot;Nevada&quot;</span>","value":"\"Nevada\""}],"value":"[1305 \"Nevada\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1395</span>","value":"1395"},{"type":"html","content":"<span class='clj-string'>&quot;TCU&quot;</span>","value":"\"TCU\""}],"value":"[1395 \"TCU\"]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1101</span>","value":"1101"},{"type":"html","content":"<span class='clj-string'>&quot;Abilene Chr&quot;</span>","value":"\"Abilene Chr\""}],"value":"[1101 \"Abilene Chr\"]"}],"value":"([1348 \"Rhode Island\"] [1244 \"Kennesaw\"] [1368 \"SE Louisiana\"] [1282 \"Missouri KC\"] [1218 \"Hawaii\"] [1425 \"USC\"] [1371 \"Seton Hall\"] [1305 \"Nevada\"] [1395 \"TCU\"] [1101 \"Abilene Chr\"])"}
;; <=

;; @@
(def slots
  (with-open [in (clojure.java.io/reader "march-machine-learning-mania-2016-v2/TourneySlots.csv")]
    (doall
      (->> (rest (csv/read-csv in))
           (reduce (fn [m [season slot hi lo]]
                     (let [keys [(read-string season) slot]
                           val [hi lo]]
                       (assoc-in m keys val)))
                   {})))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/slots</span>","value":"#'lambda-ml.examples.kaggle.march-madness/slots"}
;; <=

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.kaggle.march-madness/seeds-index</span>","value":"#'lambda-ml.examples.kaggle.march-madness/seeds-index"}
;; <=

;; @@
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
; generate lookup table from seed/slot to winning team

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
                             loteam (get-in index [lo])]
                         (if (and hiteam loteam)
                           (let [scores (get-in tourney-results [year hiteam loteam])
                                 winner (if (> (:score scores) (:oppscore scores)) hiteam loteam)]
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
    (for [[slot t] (slots year)]
      (let [[hi lo] (map index t)
            round (parse-round slot)
            hiseed (get-in seeds-index [year hi])
            loseed (get-in seeds-index [year lo])
            winner (if (= hi (index slot)) 0 1)]
        [round hi lo (teams hi) (teams lo) hiseed loseed winner]))))

(doseq [x (sort-by first (generate-matchups 2012))]
  (println x))
;; @@
;; ->
;;; [0 1249 1436 Lamar Vermont 16 16 1]
;;; [0 1143 1378 California South Florida 12 12 1]
;;; [0 1140 1233 BYU Iona 14 14 0]
;;; [0 1290 1443 MS Valley St WKU 16 16 1]
;;; [1 1199 1382 Florida St St Bonaventure 3 14 0]
;;; [1 1196 1438 Florida Virginia 7 10 0]
;;; [1 1396 1378 Temple South Florida 5 12 1]
;;; [1 1277 1254 Michigan St Long Island 1 16 0]
;;; [1 1211 1452 Gonzaga West Virginia 7 10 0]
;;; [1 1207 1125 Georgetown Belmont 3 14 0]
;;; [1 1314 1436 North Carolina Vermont 1 16 0]
;;; [1 1153 1400 Cincinnati Texas 6 11 0]
;;; [1 1323 1462 Notre Dame Xavier 7 10 1]
;;; [1 1243 1379 Kansas St Southern Miss 8 9 0]
;;; [1 1231 1308 Indiana New Mexico St 4 13 0]
;;; [1 1388 1345 St Mary&#x27;s CA Purdue 7 10 1]
;;; [1 1458 1285 Wisconsin Montana 4 13 0]
;;; [1 1124 1355 Baylor S Dakota St 3 14 0]
;;; [1 1276 1325 Michigan Ohio 4 13 1]
;;; [1 1293 1161 Murray St Colorado St 6 11 0]
;;; [1 1166 1104 Creighton Alabama 8 9 0]
;;; [1 1246 1443 Kentucky WKU 1 16 0]
;;; [1 1242 1178 Kansas Detroit 2 15 0]
;;; [1 1393 1421 Syracuse UNC Asheville 1 16 0]
;;; [1 1307 1253 New Mexico Long Beach St 5 12 0]
;;; [1 1455 1433 Wichita St VA Commonwealth 5 12 1]
;;; [1 1424 1160 UNLV Colorado 6 11 1]
;;; [1 1235 1163 Iowa St Connecticut 8 9 0]
;;; [1 1181 1250 Duke Lehigh 2 15 1]
;;; [1 1326 1259 Ohio St Loyola MD 2 15 0]
;;; [1 1257 1172 Louisville Davidson 4 13 0]
;;; [1 1435 1217 Vanderbilt Harvard 5 12 0]
;;; [1 1281 1313 Missouri Norfolk St 2 15 1]
;;; [1 1272 1387 Memphis St Louis 8 9 1]
;;; [1 1361 1301 San Diego St NC State 6 11 1]
;;; [1 1266 1140 Marquette BYU 3 14 0]
;;; [2 1207 1301 Georgetown NC State 3 11 1]
;;; [2 1266 1293 Marquette Murray St 3 6 0]
;;; [2 1458 1435 Wisconsin Vanderbilt 4 5 0]
;;; [2 1246 1235 Kentucky Iowa St 1 8 0]
;;; [2 1326 1211 Ohio St Gonzaga 2 7 0]
;;; [2 1242 1345 Kansas Purdue 2 10 0]
;;; [2 1231 1433 Indiana VA Commonwealth 4 12 0]
;;; [2 1277 1387 Michigan St St Louis 1 9 0]
;;; [2 1124 1160 Baylor Colorado 3 11 0]
;;; [2 1199 1153 Florida St Cincinnati 3 6 1]
;;; [2 1325 1378 Ohio South Florida 13 12 0]
;;; [2 1250 1462 Lehigh Xavier 15 10 1]
;;; [2 1313 1196 Norfolk St Florida 15 7 1]
;;; [2 1393 1243 Syracuse Kansas St 1 8 0]
;;; [2 1257 1307 Louisville New Mexico 4 5 0]
;;; [2 1314 1166 North Carolina Creighton 1 8 0]
;;; [3 1246 1231 Kentucky Indiana 1 4 0]
;;; [3 1277 1257 Michigan St Louisville 1 4 1]
;;; [3 1393 1458 Syracuse Wisconsin 1 4 0]
;;; [3 1326 1153 Ohio St Cincinnati 2 6 0]
;;; [3 1242 1301 Kansas NC State 2 11 0]
;;; [3 1196 1266 Florida Marquette 7 3 0]
;;; [3 1314 1325 North Carolina Ohio 1 13 0]
;;; [3 1462 1124 Xavier Baylor 10 3 1]
;;; [4 1257 1196 Louisville Florida 4 7 0]
;;; [4 1393 1326 Syracuse Ohio St 1 2 1]
;;; [4 1246 1124 Kentucky Baylor 1 3 0]
;;; [4 1314 1242 North Carolina Kansas 1 2 1]
;;; [5 1326 1242 Ohio St Kansas 2 2 1]
;;; [5 1246 1257 Kentucky Louisville 1 4 0]
;;; [6 1242 1246 Kansas Kentucky 2 1 1]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@



(def year 2012)

(slots 2012)

; round zero results
(def round0
  (let [matchups (filter (fn [[round _]] (not (= "R" (subs round 0 1)))) (slots year))]
    (for [[round [hi lo]] matchups]
      (let [hiteam (get-in seeds [year hi])
            loteam (get-in seeds [year lo])
            scores (get-in tourney-results [year hiteam loteam])
            winner (if (> (:score scores) (:oppscore scores)) 0 1)]
        [0 hiteam loteam (teams hiteam) (teams loteam) (seed hi) (seed lo) winner]))))

(take 2 round0)

; round one results
(let [matchups (filter (fn [[round _]] (= "R1" (subs round 0 2))) (slots year))]
  (for [[round [hi lo]] matchups]
    (let [hiteam (get-in seeds [year hi])
          loteam (get-in seeds [year lo])]
      [0 hiteam loteam (teams hiteam) (teams loteam) (seed hi) (seed lo)])))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1199</span>","value":"1199"},{"type":"html","content":"<span class='clj-long'>1382</span>","value":"1382"},{"type":"html","content":"<span class='clj-string'>&quot;Florida St&quot;</span>","value":"\"Florida St\""},{"type":"html","content":"<span class='clj-string'>&quot;St Bonaventure&quot;</span>","value":"\"St Bonaventure\""},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>14</span>","value":"14"}],"value":"[0 1199 1382 \"Florida St\" \"St Bonaventure\" 3 14]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1196</span>","value":"1196"},{"type":"html","content":"<span class='clj-long'>1438</span>","value":"1438"},{"type":"html","content":"<span class='clj-string'>&quot;Florida&quot;</span>","value":"\"Florida\""},{"type":"html","content":"<span class='clj-string'>&quot;Virginia&quot;</span>","value":"\"Virginia\""},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"}],"value":"[0 1196 1438 \"Florida\" \"Virginia\" 7 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1396</span>","value":"1396"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-string'>&quot;Temple&quot;</span>","value":"\"Temple\""},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>12</span>","value":"12"}],"value":"[0 1396 nil \"Temple\" nil 5 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1277</span>","value":"1277"},{"type":"html","content":"<span class='clj-long'>1254</span>","value":"1254"},{"type":"html","content":"<span class='clj-string'>&quot;Michigan St&quot;</span>","value":"\"Michigan St\""},{"type":"html","content":"<span class='clj-string'>&quot;Long Island&quot;</span>","value":"\"Long Island\""},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>16</span>","value":"16"}],"value":"[0 1277 1254 \"Michigan St\" \"Long Island\" 1 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1211</span>","value":"1211"},{"type":"html","content":"<span class='clj-long'>1452</span>","value":"1452"},{"type":"html","content":"<span class='clj-string'>&quot;Gonzaga&quot;</span>","value":"\"Gonzaga\""},{"type":"html","content":"<span class='clj-string'>&quot;West Virginia&quot;</span>","value":"\"West Virginia\""},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"}],"value":"[0 1211 1452 \"Gonzaga\" \"West Virginia\" 7 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1207</span>","value":"1207"},{"type":"html","content":"<span class='clj-long'>1125</span>","value":"1125"},{"type":"html","content":"<span class='clj-string'>&quot;Georgetown&quot;</span>","value":"\"Georgetown\""},{"type":"html","content":"<span class='clj-string'>&quot;Belmont&quot;</span>","value":"\"Belmont\""},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>14</span>","value":"14"}],"value":"[0 1207 1125 \"Georgetown\" \"Belmont\" 3 14]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1314</span>","value":"1314"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-string'>&quot;North Carolina&quot;</span>","value":"\"North Carolina\""},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>16</span>","value":"16"}],"value":"[0 1314 nil \"North Carolina\" nil 1 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1153</span>","value":"1153"},{"type":"html","content":"<span class='clj-long'>1400</span>","value":"1400"},{"type":"html","content":"<span class='clj-string'>&quot;Cincinnati&quot;</span>","value":"\"Cincinnati\""},{"type":"html","content":"<span class='clj-string'>&quot;Texas&quot;</span>","value":"\"Texas\""},{"type":"html","content":"<span class='clj-unkown'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>11</span>","value":"11"}],"value":"[0 1153 1400 \"Cincinnati\" \"Texas\" 6 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1323</span>","value":"1323"},{"type":"html","content":"<span class='clj-long'>1462</span>","value":"1462"},{"type":"html","content":"<span class='clj-string'>&quot;Notre Dame&quot;</span>","value":"\"Notre Dame\""},{"type":"html","content":"<span class='clj-string'>&quot;Xavier&quot;</span>","value":"\"Xavier\""},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"}],"value":"[0 1323 1462 \"Notre Dame\" \"Xavier\" 7 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1243</span>","value":"1243"},{"type":"html","content":"<span class='clj-long'>1379</span>","value":"1379"},{"type":"html","content":"<span class='clj-string'>&quot;Kansas St&quot;</span>","value":"\"Kansas St\""},{"type":"html","content":"<span class='clj-string'>&quot;Southern Miss&quot;</span>","value":"\"Southern Miss\""},{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"}],"value":"[0 1243 1379 \"Kansas St\" \"Southern Miss\" 8 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1231</span>","value":"1231"},{"type":"html","content":"<span class='clj-long'>1308</span>","value":"1308"},{"type":"html","content":"<span class='clj-string'>&quot;Indiana&quot;</span>","value":"\"Indiana\""},{"type":"html","content":"<span class='clj-string'>&quot;New Mexico St&quot;</span>","value":"\"New Mexico St\""},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>13</span>","value":"13"}],"value":"[0 1231 1308 \"Indiana\" \"New Mexico St\" 4 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1388</span>","value":"1388"},{"type":"html","content":"<span class='clj-long'>1345</span>","value":"1345"},{"type":"html","content":"<span class='clj-string'>&quot;St Mary&#x27;s CA&quot;</span>","value":"\"St Mary's CA\""},{"type":"html","content":"<span class='clj-string'>&quot;Purdue&quot;</span>","value":"\"Purdue\""},{"type":"html","content":"<span class='clj-unkown'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>10</span>","value":"10"}],"value":"[0 1388 1345 \"St Mary's CA\" \"Purdue\" 7 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1458</span>","value":"1458"},{"type":"html","content":"<span class='clj-long'>1285</span>","value":"1285"},{"type":"html","content":"<span class='clj-string'>&quot;Wisconsin&quot;</span>","value":"\"Wisconsin\""},{"type":"html","content":"<span class='clj-string'>&quot;Montana&quot;</span>","value":"\"Montana\""},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>13</span>","value":"13"}],"value":"[0 1458 1285 \"Wisconsin\" \"Montana\" 4 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1124</span>","value":"1124"},{"type":"html","content":"<span class='clj-long'>1355</span>","value":"1355"},{"type":"html","content":"<span class='clj-string'>&quot;Baylor&quot;</span>","value":"\"Baylor\""},{"type":"html","content":"<span class='clj-string'>&quot;S Dakota St&quot;</span>","value":"\"S Dakota St\""},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>14</span>","value":"14"}],"value":"[0 1124 1355 \"Baylor\" \"S Dakota St\" 3 14]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1276</span>","value":"1276"},{"type":"html","content":"<span class='clj-long'>1325</span>","value":"1325"},{"type":"html","content":"<span class='clj-string'>&quot;Michigan&quot;</span>","value":"\"Michigan\""},{"type":"html","content":"<span class='clj-string'>&quot;Ohio&quot;</span>","value":"\"Ohio\""},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>13</span>","value":"13"}],"value":"[0 1276 1325 \"Michigan\" \"Ohio\" 4 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1293</span>","value":"1293"},{"type":"html","content":"<span class='clj-long'>1161</span>","value":"1161"},{"type":"html","content":"<span class='clj-string'>&quot;Murray St&quot;</span>","value":"\"Murray St\""},{"type":"html","content":"<span class='clj-string'>&quot;Colorado St&quot;</span>","value":"\"Colorado St\""},{"type":"html","content":"<span class='clj-unkown'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>11</span>","value":"11"}],"value":"[0 1293 1161 \"Murray St\" \"Colorado St\" 6 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1166</span>","value":"1166"},{"type":"html","content":"<span class='clj-long'>1104</span>","value":"1104"},{"type":"html","content":"<span class='clj-string'>&quot;Creighton&quot;</span>","value":"\"Creighton\""},{"type":"html","content":"<span class='clj-string'>&quot;Alabama&quot;</span>","value":"\"Alabama\""},{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"}],"value":"[0 1166 1104 \"Creighton\" \"Alabama\" 8 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1246</span>","value":"1246"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-string'>&quot;Kentucky&quot;</span>","value":"\"Kentucky\""},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>16</span>","value":"16"}],"value":"[0 1246 nil \"Kentucky\" nil 1 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1242</span>","value":"1242"},{"type":"html","content":"<span class='clj-long'>1178</span>","value":"1178"},{"type":"html","content":"<span class='clj-string'>&quot;Kansas&quot;</span>","value":"\"Kansas\""},{"type":"html","content":"<span class='clj-string'>&quot;Detroit&quot;</span>","value":"\"Detroit\""},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>15</span>","value":"15"}],"value":"[0 1242 1178 \"Kansas\" \"Detroit\" 2 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1393</span>","value":"1393"},{"type":"html","content":"<span class='clj-long'>1421</span>","value":"1421"},{"type":"html","content":"<span class='clj-string'>&quot;Syracuse&quot;</span>","value":"\"Syracuse\""},{"type":"html","content":"<span class='clj-string'>&quot;UNC Asheville&quot;</span>","value":"\"UNC Asheville\""},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>16</span>","value":"16"}],"value":"[0 1393 1421 \"Syracuse\" \"UNC Asheville\" 1 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1307</span>","value":"1307"},{"type":"html","content":"<span class='clj-long'>1253</span>","value":"1253"},{"type":"html","content":"<span class='clj-string'>&quot;New Mexico&quot;</span>","value":"\"New Mexico\""},{"type":"html","content":"<span class='clj-string'>&quot;Long Beach St&quot;</span>","value":"\"Long Beach St\""},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>12</span>","value":"12"}],"value":"[0 1307 1253 \"New Mexico\" \"Long Beach St\" 5 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1455</span>","value":"1455"},{"type":"html","content":"<span class='clj-long'>1433</span>","value":"1433"},{"type":"html","content":"<span class='clj-string'>&quot;Wichita St&quot;</span>","value":"\"Wichita St\""},{"type":"html","content":"<span class='clj-string'>&quot;VA Commonwealth&quot;</span>","value":"\"VA Commonwealth\""},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>12</span>","value":"12"}],"value":"[0 1455 1433 \"Wichita St\" \"VA Commonwealth\" 5 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1424</span>","value":"1424"},{"type":"html","content":"<span class='clj-long'>1160</span>","value":"1160"},{"type":"html","content":"<span class='clj-string'>&quot;UNLV&quot;</span>","value":"\"UNLV\""},{"type":"html","content":"<span class='clj-string'>&quot;Colorado&quot;</span>","value":"\"Colorado\""},{"type":"html","content":"<span class='clj-unkown'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>11</span>","value":"11"}],"value":"[0 1424 1160 \"UNLV\" \"Colorado\" 6 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1235</span>","value":"1235"},{"type":"html","content":"<span class='clj-long'>1163</span>","value":"1163"},{"type":"html","content":"<span class='clj-string'>&quot;Iowa St&quot;</span>","value":"\"Iowa St\""},{"type":"html","content":"<span class='clj-string'>&quot;Connecticut&quot;</span>","value":"\"Connecticut\""},{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"}],"value":"[0 1235 1163 \"Iowa St\" \"Connecticut\" 8 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1181</span>","value":"1181"},{"type":"html","content":"<span class='clj-long'>1250</span>","value":"1250"},{"type":"html","content":"<span class='clj-string'>&quot;Duke&quot;</span>","value":"\"Duke\""},{"type":"html","content":"<span class='clj-string'>&quot;Lehigh&quot;</span>","value":"\"Lehigh\""},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>15</span>","value":"15"}],"value":"[0 1181 1250 \"Duke\" \"Lehigh\" 2 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1326</span>","value":"1326"},{"type":"html","content":"<span class='clj-long'>1259</span>","value":"1259"},{"type":"html","content":"<span class='clj-string'>&quot;Ohio St&quot;</span>","value":"\"Ohio St\""},{"type":"html","content":"<span class='clj-string'>&quot;Loyola MD&quot;</span>","value":"\"Loyola MD\""},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>15</span>","value":"15"}],"value":"[0 1326 1259 \"Ohio St\" \"Loyola MD\" 2 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1257</span>","value":"1257"},{"type":"html","content":"<span class='clj-long'>1172</span>","value":"1172"},{"type":"html","content":"<span class='clj-string'>&quot;Louisville&quot;</span>","value":"\"Louisville\""},{"type":"html","content":"<span class='clj-string'>&quot;Davidson&quot;</span>","value":"\"Davidson\""},{"type":"html","content":"<span class='clj-unkown'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>13</span>","value":"13"}],"value":"[0 1257 1172 \"Louisville\" \"Davidson\" 4 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1435</span>","value":"1435"},{"type":"html","content":"<span class='clj-long'>1217</span>","value":"1217"},{"type":"html","content":"<span class='clj-string'>&quot;Vanderbilt&quot;</span>","value":"\"Vanderbilt\""},{"type":"html","content":"<span class='clj-string'>&quot;Harvard&quot;</span>","value":"\"Harvard\""},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>12</span>","value":"12"}],"value":"[0 1435 1217 \"Vanderbilt\" \"Harvard\" 5 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1281</span>","value":"1281"},{"type":"html","content":"<span class='clj-long'>1313</span>","value":"1313"},{"type":"html","content":"<span class='clj-string'>&quot;Missouri&quot;</span>","value":"\"Missouri\""},{"type":"html","content":"<span class='clj-string'>&quot;Norfolk St&quot;</span>","value":"\"Norfolk St\""},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>15</span>","value":"15"}],"value":"[0 1281 1313 \"Missouri\" \"Norfolk St\" 2 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1272</span>","value":"1272"},{"type":"html","content":"<span class='clj-long'>1387</span>","value":"1387"},{"type":"html","content":"<span class='clj-string'>&quot;Memphis&quot;</span>","value":"\"Memphis\""},{"type":"html","content":"<span class='clj-string'>&quot;St Louis&quot;</span>","value":"\"St Louis\""},{"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"}],"value":"[0 1272 1387 \"Memphis\" \"St Louis\" 8 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1361</span>","value":"1361"},{"type":"html","content":"<span class='clj-long'>1301</span>","value":"1301"},{"type":"html","content":"<span class='clj-string'>&quot;San Diego St&quot;</span>","value":"\"San Diego St\""},{"type":"html","content":"<span class='clj-string'>&quot;NC State&quot;</span>","value":"\"NC State\""},{"type":"html","content":"<span class='clj-unkown'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>11</span>","value":"11"}],"value":"[0 1361 1301 \"San Diego St\" \"NC State\" 6 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1266</span>","value":"1266"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-string'>&quot;Marquette&quot;</span>","value":"\"Marquette\""},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>14</span>","value":"14"}],"value":"[0 1266 nil \"Marquette\" nil 3 14]"}],"value":"([0 1199 1382 \"Florida St\" \"St Bonaventure\" 3 14] [0 1196 1438 \"Florida\" \"Virginia\" 7 10] [0 1396 nil \"Temple\" nil 5 12] [0 1277 1254 \"Michigan St\" \"Long Island\" 1 16] [0 1211 1452 \"Gonzaga\" \"West Virginia\" 7 10] [0 1207 1125 \"Georgetown\" \"Belmont\" 3 14] [0 1314 nil \"North Carolina\" nil 1 16] [0 1153 1400 \"Cincinnati\" \"Texas\" 6 11] [0 1323 1462 \"Notre Dame\" \"Xavier\" 7 10] [0 1243 1379 \"Kansas St\" \"Southern Miss\" 8 9] [0 1231 1308 \"Indiana\" \"New Mexico St\" 4 13] [0 1388 1345 \"St Mary's CA\" \"Purdue\" 7 10] [0 1458 1285 \"Wisconsin\" \"Montana\" 4 13] [0 1124 1355 \"Baylor\" \"S Dakota St\" 3 14] [0 1276 1325 \"Michigan\" \"Ohio\" 4 13] [0 1293 1161 \"Murray St\" \"Colorado St\" 6 11] [0 1166 1104 \"Creighton\" \"Alabama\" 8 9] [0 1246 nil \"Kentucky\" nil 1 16] [0 1242 1178 \"Kansas\" \"Detroit\" 2 15] [0 1393 1421 \"Syracuse\" \"UNC Asheville\" 1 16] [0 1307 1253 \"New Mexico\" \"Long Beach St\" 5 12] [0 1455 1433 \"Wichita St\" \"VA Commonwealth\" 5 12] [0 1424 1160 \"UNLV\" \"Colorado\" 6 11] [0 1235 1163 \"Iowa St\" \"Connecticut\" 8 9] [0 1181 1250 \"Duke\" \"Lehigh\" 2 15] [0 1326 1259 \"Ohio St\" \"Loyola MD\" 2 15] [0 1257 1172 \"Louisville\" \"Davidson\" 4 13] [0 1435 1217 \"Vanderbilt\" \"Harvard\" 5 12] [0 1281 1313 \"Missouri\" \"Norfolk St\" 2 15] [0 1272 1387 \"Memphis\" \"St Louis\" 8 9] [0 1361 1301 \"San Diego St\" \"NC State\" 6 11] [0 1266 nil \"Marquette\" nil 3 14])"}
;; <=

;; @@

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>8</span>","value":"8"}
;; <=

;; @@

;; @@
