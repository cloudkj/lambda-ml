;; gorilla-repl.fileformat = 1

;; **
;;; # Lambda ML Example: DBSCAN
;; **

;; @@
(ns lambda-ml.examples.worksheets.dbscan
  (require [lambda-ml.clustering.dbscan :refer :all]
           [lambda-ml.distance :refer :all]
           [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def points [[64.22906466107816  21.979356040013954]
             [9.502019068226218  73.5146190142259]
             [73.467643359676    49.11882050731219]
             [43.89991499437019  30.877086140215397]
             [31.66112900408916  62.006799353519455]
             [32.96188162290491  62.647924402495846]
             [29.860327935311943 61.4603339463938]
             [30.252436050213873 61.628358017420396]
             [29.478669344214723 63.34734829352237]
             [31.20809231721796  60.62778950878419]
             [56.719979556510225 12.79888943536207]
             [58.65873904968612  12.760463243583217]
             [56.961131111718714 13.99614970618859]
             [56.45650068600662  14.442072300706059]
             [56.4971734713763   15.955536322668195]
             [56.84485225030221  14.559824497206137]
             [55.51035064924508  11.432733331574127]
             [56.13687033439055  10.726739906473085]
             [55.76618270971812  11.259738458553329]
             [18.786322022089735 39.41377491992898]
             [17.121476165484637 39.838845615973696]
             [19.683527131564915 39.34956996375695]])

(def xmax (apply max (map first points)))
(def ymax (apply max (map second points)))
(def range [[0 xmax] [0 ymax]])

(plot/list-plot points :plot-range range)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,73.467643359676]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,73.5146190142259]}],"marks":[{"type":"symbol","from":{"data":"90d8a79c-53c6-423a-a5c9-c04c376dc7d1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"data":[{"name":"90d8a79c-53c6-423a-a5c9-c04c376dc7d1","values":[{"x":64.22906466107816,"y":21.979356040013954},{"x":9.502019068226218,"y":73.5146190142259},{"x":73.467643359676,"y":49.11882050731219},{"x":43.89991499437019,"y":30.877086140215397},{"x":31.66112900408916,"y":62.006799353519455},{"x":32.96188162290491,"y":62.647924402495846},{"x":29.860327935311943,"y":61.4603339463938},{"x":30.252436050213873,"y":61.628358017420396},{"x":29.478669344214723,"y":63.34734829352237},{"x":31.20809231721796,"y":60.62778950878419},{"x":56.719979556510225,"y":12.79888943536207},{"x":58.65873904968612,"y":12.760463243583217},{"x":56.961131111718714,"y":13.99614970618859},{"x":56.45650068600662,"y":14.442072300706059},{"x":56.4971734713763,"y":15.955536322668195},{"x":56.84485225030221,"y":14.559824497206137},{"x":55.51035064924508,"y":11.432733331574127},{"x":56.13687033439055,"y":10.726739906473085},{"x":55.76618270971812,"y":11.259738458553329},{"x":18.786322022089735,"y":39.41377491992898},{"x":17.121476165484637,"y":39.838845615973696},{"x":19.683527131564915,"y":39.34956996375695}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 73.467643359676]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 73.5146190142259]}], :marks [{:type \"symbol\", :from {:data \"90d8a79c-53c6-423a-a5c9-c04c376dc7d1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :data [{:name \"90d8a79c-53c6-423a-a5c9-c04c376dc7d1\", :values ({:x 64.22906466107816, :y 21.979356040013954} {:x 9.502019068226218, :y 73.5146190142259} {:x 73.467643359676, :y 49.11882050731219} {:x 43.89991499437019, :y 30.877086140215397} {:x 31.66112900408916, :y 62.006799353519455} {:x 32.96188162290491, :y 62.647924402495846} {:x 29.860327935311943, :y 61.4603339463938} {:x 30.252436050213873, :y 61.628358017420396} {:x 29.478669344214723, :y 63.34734829352237} {:x 31.20809231721796, :y 60.62778950878419} {:x 56.719979556510225, :y 12.79888943536207} {:x 58.65873904968612, :y 12.760463243583217} {:x 56.961131111718714, :y 13.99614970618859} {:x 56.45650068600662, :y 14.442072300706059} {:x 56.4971734713763, :y 15.955536322668195} {:x 56.84485225030221, :y 14.559824497206137} {:x 55.51035064924508, :y 11.432733331574127} {:x 56.13687033439055, :y 10.726739906473085} {:x 55.76618270971812, :y 11.259738458553329} {:x 18.786322022089735, :y 39.41377491992898} {:x 17.121476165484637, :y 39.838845615973696} {:x 19.683527131564915, :y 39.34956996375695})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=

;; @@
(def epsilon 4.0)
(def min-pts 2)
(def clustering (dbscan euclidean epsilon min-pts points))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;lambda-ml.examples.worksheets.dbscan/clustering</span>","value":"#'lambda-ml.examples.worksheets.dbscan/clustering"}
;; <=

;; @@
(def colors {1 "red" 2 "green" 3 "blue"})

(apply plot/compose
       (conj (for [[id pts] (group-by val clustering)]
               (plot/list-plot (map first pts)
                               :color (get colors id)
                               :plot-range range))
             (plot/list-plot (filter #(not (contains? clustering %)) points)
                             :plot-range range)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[0,73.467643359676]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,73.5146190142259]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"8e0bab70-3c97-4e9d-97da-7e77c9722815","values":[{"x":64.22906466107816,"y":21.979356040013954},{"x":9.502019068226218,"y":73.5146190142259},{"x":73.467643359676,"y":49.11882050731219},{"x":43.89991499437019,"y":30.877086140215397}]},{"name":"28e4f5d0-58dc-4b42-a76f-ba53c5e8bd74","values":[{"x":56.961131111718714,"y":13.99614970618859},{"x":56.84485225030221,"y":14.559824497206137},{"x":56.45650068600662,"y":14.442072300706059},{"x":56.4971734713763,"y":15.955536322668195},{"x":55.51035064924508,"y":11.432733331574127},{"x":58.65873904968612,"y":12.760463243583217},{"x":56.719979556510225,"y":12.79888943536207},{"x":55.76618270971812,"y":11.259738458553329},{"x":56.13687033439055,"y":10.726739906473085}]},{"name":"0b6bcc10-32da-451e-820d-1825d949cf76","values":[{"x":29.478669344214723,"y":63.34734829352237},{"x":32.96188162290491,"y":62.647924402495846},{"x":29.860327935311943,"y":61.4603339463938},{"x":30.252436050213873,"y":61.628358017420396},{"x":31.66112900408916,"y":62.006799353519455},{"x":31.20809231721796,"y":60.62778950878419}]},{"name":"68ac99fa-f909-4678-b1ae-f55e05d75e2c","values":[{"x":19.683527131564915,"y":39.34956996375695},{"x":18.786322022089735,"y":39.41377491992898},{"x":17.121476165484637,"y":39.838845615973696}]}],"marks":[{"type":"symbol","from":{"data":"8e0bab70-3c97-4e9d-97da-7e77c9722815"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"28e4f5d0-58dc-4b42-a76f-ba53c5e8bd74"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"green"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"0b6bcc10-32da-451e-820d-1825d949cf76"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"red"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"68ac99fa-f909-4678-b1ae-f55e05d75e2c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"blue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [0 73.467643359676]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 73.5146190142259]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"8e0bab70-3c97-4e9d-97da-7e77c9722815\", :values ({:x 64.22906466107816, :y 21.979356040013954} {:x 9.502019068226218, :y 73.5146190142259} {:x 73.467643359676, :y 49.11882050731219} {:x 43.89991499437019, :y 30.877086140215397})} {:name \"28e4f5d0-58dc-4b42-a76f-ba53c5e8bd74\", :values ({:x 56.961131111718714, :y 13.99614970618859} {:x 56.84485225030221, :y 14.559824497206137} {:x 56.45650068600662, :y 14.442072300706059} {:x 56.4971734713763, :y 15.955536322668195} {:x 55.51035064924508, :y 11.432733331574127} {:x 58.65873904968612, :y 12.760463243583217} {:x 56.719979556510225, :y 12.79888943536207} {:x 55.76618270971812, :y 11.259738458553329} {:x 56.13687033439055, :y 10.726739906473085})} {:name \"0b6bcc10-32da-451e-820d-1825d949cf76\", :values ({:x 29.478669344214723, :y 63.34734829352237} {:x 32.96188162290491, :y 62.647924402495846} {:x 29.860327935311943, :y 61.4603339463938} {:x 30.252436050213873, :y 61.628358017420396} {:x 31.66112900408916, :y 62.006799353519455} {:x 31.20809231721796, :y 60.62778950878419})} {:name \"68ac99fa-f909-4678-b1ae-f55e05d75e2c\", :values ({:x 19.683527131564915, :y 39.34956996375695} {:x 18.786322022089735, :y 39.41377491992898} {:x 17.121476165484637, :y 39.838845615973696})}), :marks ({:type \"symbol\", :from {:data \"8e0bab70-3c97-4e9d-97da-7e77c9722815\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"28e4f5d0-58dc-4b42-a76f-ba53c5e8bd74\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"green\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"0b6bcc10-32da-451e-820d-1825d949cf76\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"red\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"68ac99fa-f909-4678-b1ae-f55e05d75e2c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"blue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
;; <=
