(ns small.pqueue)

;; Priority queue (bounded and unbounded)

(defn make-queue
  []
  (vector))

(defn make-item
  [value priority]
  (vector priority value))

(defn item-priority
  [item]
  (first item))

(defn item-value
  [item]
  (second item))

(defn insert
  ([queue value priority]
   (insert queue value priority Integer/MAX_VALUE))
  ([queue value priority bound]
   (let [full (>= (count queue) bound)]
     (if (and full (>= priority (item-priority (peek queue))))
       queue
       (let [index (loop [lo 0
                          hi (count queue)]
                     (if (>= lo hi)
                       lo
                       (let [mid (quot (+ lo hi) 2)]
                         (if (< priority (item-priority (nth queue mid)))
                           (recur lo mid)
                           (recur (+ mid 1) hi)))))
             item (make-item value priority)
             end (if full (dec (count queue)) (count queue))]
         (apply conj
                (subvec queue 0 index)
                item
                (subvec queue index end)))))))
