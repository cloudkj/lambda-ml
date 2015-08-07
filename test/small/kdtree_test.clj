(ns small.kdtree-test
  (:require [clojure.test :refer :all]
            [small.kdtree :refer :all]))

(deftest test-kd-tree
  (let [tree (make-tree 2 [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= (get-value tree) [7 2]))
    (is (= (get-path tree [:left])  (get-left tree)))
    (is (= (get-path tree [:right]) (get-right tree)))
    (is (= (get-path tree [:left :left])  (-> tree get-left get-left)))
    (is (= (get-path tree [:left :right]) (-> tree get-left get-right)))
    (is (= (get-path tree [:right :left]) (-> tree get-right get-left)))
    (is (= (get-value (get-path tree [:left]))         [5 4]))
    (is (= (get-value (get-path tree [:right]))        [9 6]))
    (is (= (get-value (get-path tree [:left  :left]))  [2 3]))
    (is (= (get-value (get-path tree [:left  :right])) [4 7]))
    (is (= (get-value (get-path tree [:right :left]))  [8 1]))
    (is (nil? (get-path tree [:left :left :left])))
    (is (nil? (get-path tree [:left :left :right])))
    (is (nil? (get-path tree [:left :right :left])))
    (is (nil? (get-path tree [:left :right :right])))
    (is (nil? (get-path tree [:right :left :left])))
    (is (nil? (get-path tree [:right :left :right])))
    (is (nil? (get-path tree [:right :right])))))
