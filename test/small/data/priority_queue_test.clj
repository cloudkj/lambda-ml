(ns small.data.priority-queue-test
  (:require [clojure.test :refer :all]
            [small.data.priority-queue :refer :all]))

(deftest test-priority-queue
  (let [queue (-> (make-queue)
                  (insert :e 4.6)
                  (insert :d 3.2)
                  (insert :a 0.1)
                  (insert :c 1.33)
                  (insert :b 0.25))]
    (is (= (map item-value queue)
           [:a :b :c :d :e]))
    (is (= (map item-value (insert queue :f 0.4))
           [:a :b :f :c :d :e]))))

(deftest test-priority-queue-bounded
  (let [bound 5
        queue (-> (make-queue)
                  (insert :e 4.6)
                  (insert :d 3.2)
                  (insert :a 0.1)
                  (insert :c 1.33)
                  (insert :b 0.25))]
    (is (= (map item-value (insert queue :f 0.4 bound))
           [:a :b :f :c :d]))
    (is (= (map item-value (insert queue :g 4.0 bound))
           [:a :b :c :d :g]))))
